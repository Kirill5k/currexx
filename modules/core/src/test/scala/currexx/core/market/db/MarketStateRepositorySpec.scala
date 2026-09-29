package currexx.core.market.db

import cats.effect.{IO, Ref}
import cats.effect.unsafe.IORuntime
import cats.data.NonEmptyList
import cats.syntax.all.*
import currexx.core.{MockActionDispatcher, MongoSpec}
import currexx.core.common.action.Action
import currexx.core.common.db.Repository
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.core.market.*
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.{Condition, Direction, ValueRole}
import currexx.domain.user.UserId
import kirill5k.common.cats.Clock
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Update}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import scala.concurrent.Future

class MarketStateRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12354

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val ts = Instant.parse("2026-01-02T20:00:00Z")

  given Clock[IO] = Clock.mock[IO](ts)

  val initial: MarketState = MarketState.initial(Users.uid, Markets.gbpeur, ts)

  "A MarketStateRepository" when {
    "save" should {
      "create a state that has not been stored yet" in withEmbeddedMongoDb { db =>
        val state  = initial.copy(profile = Markets.profile, previousProfile = Some(MarketProfile()), lastCandleTime = Some(ts))
        val result = for
          repo     <- MarketStateRepository.make(db)
          accepted <- repo.save(state)
          res      <- repo.find(Users.uid, Markets.gbpeur)
        yield (accepted, res)

        result.map { case (accepted, res) =>
          accepted mustBe true
          res.map(_.withTime(ts)) mustBe Some(state.copy(version = Some(1)))
        }
      }

      "update an existing state when the version matches and increment it" in withEmbeddedMongoDb { db =>
        val result = for
          repo     <- MarketStateRepository.make(db)
          _        <- repo.save(initial)
          created  <- repo.find(Users.uid, Markets.gbpeur).map(_.get)
          accepted <- repo.save(created.copy(profile = Markets.profile, lastTimeStateCandle = Some(ts)))
          res      <- repo.find(Users.uid, Markets.gbpeur)
        yield (accepted, res)

        result.map { case (accepted, res) =>
          accepted mustBe true
          res.map(_.profile) mustBe Some(Markets.profile)
          res.flatMap(_.lastTimeStateCandle) mustBe Some(ts)
          res.flatMap(_.version) mustBe Some(2)
        }
      }

      "reject a state read before another write" in withEmbeddedMongoDb { db =>
        val result = for
          repo     <- MarketStateRepository.make(db)
          _        <- repo.save(initial)
          stale    <- repo.find(Users.uid, Markets.gbpeur).map(_.get)
          _        <- repo.save(stale.copy(lastCandleTime = Some(ts)))
          current  <- repo.find(Users.uid, Markets.gbpeur)
          accepted <- repo.save(stale.copy(profile = Markets.profile))
          res      <- repo.find(Users.uid, Markets.gbpeur)
        yield (accepted, current, res)

        result.map { case (accepted, current, res) =>
          accepted mustBe false
          res mustBe current
        }
      }

      "reject a state read before a position update" in withEmbeddedMongoDb { db =>
        val result = for
          repo     <- MarketStateRepository.make(db)
          _        <- repo.save(initial)
          stale    <- repo.find(Users.uid, Markets.gbpeur).map(_.get)
          _        <- repo.update(Users.uid, Markets.gbpeur, Some(Markets.positionState))
          accepted <- repo.save(stale.copy(profile = Markets.profile))
          res      <- repo.find(Users.uid, Markets.gbpeur)
        yield (accepted, res)

        result.map { case (accepted, res) =>
          accepted mustBe false
          res.flatMap(_.currentPosition) mustBe Some(Markets.positionState)
          res.map(_.profile) mustBe Some(MarketProfile())
        }
      }

      "accept only one of several concurrent writes from the same version" in withEmbeddedMongoDb { db =>
        List(false, true)
          .traverse { existing =>
            val cp = if (existing) Markets.gbpusd else Markets.gbpeur
            for
              repo     <- MarketStateRepository.make(db)
              _        <- IO.whenA(existing)(repo.save(initial.copy(currencyPair = cp)).void)
              base     <- repo.find(Users.uid, cp).map(_.getOrElse(initial.copy(currencyPair = cp)))
              accepted <- List.tabulate(8)(i => repo.save(base.copy(lastCandleTime = Some(ts.plusSeconds(i))))).parSequence
              states   <- repo.getAll(Users.uid).map(_.filter(_.currencyPair == cp))
            yield (accepted.count(identity), states.size)
          }
          .map(_ mustBe List((1, 1), (1, 1)))
      }

      "save a state stored before versioning as version 0" in withEmbeddedMongoDb { db =>
        val result = for
          repo     <- MarketStateRepository.make(db)
          legacy   <- storeLegacyState(db, repo)
          accepted <- repo.save(legacy.copy(profile = Markets.profile))
          res      <- repo.find(Users.uid, Markets.gbpeur)
        yield (legacy, accepted, res)

        result.map { case (legacy, accepted, res) =>
          legacy.version mustBe Some(0)
          accepted mustBe true
          res.flatMap(_.version) mustBe Some(1)
          res.map(_.profile) mustBe Some(Markets.profile)
        }
      }

      "not recreate a state stored before versioning from a snapshot read before its deletion" in withEmbeddedMongoDb { db =>
        val result = for
          repo     <- MarketStateRepository.make(db)
          legacy   <- storeLegacyState(db, repo)
          _        <- repo.delete(Users.uid, Markets.gbpeur)
          accepted <- repo.save(legacy.copy(profile = Markets.profile))
          res      <- repo.find(Users.uid, Markets.gbpeur)
        yield (accepted, res)

        result.map { case (accepted, res) =>
          accepted mustBe false
          res mustBe None
        }
      }

      "reject a snapshot of a state that was deleted and recreated at the same version" in withEmbeddedMongoDb { db =>
        val result = for
          repo      <- MarketStateRepository.make(db)
          _         <- repo.save(initial)
          stale     <- repo.find(Users.uid, Markets.gbpeur).map(_.get)
          _         <- repo.delete(Users.uid, Markets.gbpeur)
          _         <- repo.save(initial.copy(createdAt = ts.plusSeconds(3600), lastCandleTime = Some(ts)))
          recreated <- repo.find(Users.uid, Markets.gbpeur)
          accepted  <- repo.save(stale.copy(profile = Markets.profile))
          res       <- repo.find(Users.uid, Markets.gbpeur)
        yield (recreated, accepted, res)

        result.map { case (recreated, accepted, res) =>
          recreated.flatMap(_.version) mustBe Some(1)
          accepted mustBe false
          res mustBe recreated
        }
      }

      "not recreate a state deleted after it was read" in withEmbeddedMongoDb { db =>
        val result = for
          repo     <- MarketStateRepository.make(db)
          _        <- repo.save(initial)
          stale    <- repo.find(Users.uid, Markets.gbpeur).map(_.get)
          _        <- repo.delete(Users.uid, Markets.gbpeur)
          accepted <- repo.save(stale.copy(profile = Markets.profile))
          res      <- repo.find(Users.uid, Markets.gbpeur)
        yield (accepted, res)

        result.map { case (accepted, res) =>
          accepted mustBe false
          res mustBe None
        }
      }
    }

    "used by MarketService" should {
      "reject older and repeated candles without changing state or evaluating rules" in withEmbeddedMongoDb { db =>
        val dispatcher = MockActionDispatcher[IO]
        val newer      = Signals.trend(Direction.Upward, time = ts.plusSeconds(3600))
        val older      = Signals.trend(Direction.Downward, time = ts)
        val result     = for
          repo          <- MarketStateRepository.make(db)
          service       <- MarketService.make[IO](repo, dispatcher)
          _             <- service.processSignals(Users.uid, Markets.gbpeur, List(newer))
          acceptedState <- repo.find(Users.uid, Markets.gbpeur)
          _             <- service.processSignals(Users.uid, Markets.gbpeur, List(older))
          _             <- service.processSignals(Users.uid, Markets.gbpeur, List(newer))
          state         <- repo.find(Users.uid, Markets.gbpeur)
        yield (acceptedState, state)

        result.map { case (acceptedState, state) =>
          state mustBe acceptedState
          state.flatMap(_.profile.trend) mustBe Some(TrendState(Direction.Upward, newer.time))
          dispatcher.submittedActions.toList mustBe List(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        }
      }

      "keep a manual signal's wall-clock time out of the candle watermark" in withEmbeddedMongoDb { db =>
        val previousCandle  = Instant.parse("2026-01-05T09:00:00Z")
        val completedCandle = Instant.parse("2026-01-05T10:00:00Z")
        val manualSignal    = Signals.trend(Direction.Upward, time = Instant.parse("2026-01-05T10:37:00Z"))
        val candleSignal    =
          Signals.make(Users.uid, completedCandle, Markets.gbpeur, Condition.ValueUpdated(ValueRole.Price, BigDecimal("1.25")))
        val dispatcher = MockActionDispatcher[IO]
        val result     = for
          repo        <- MarketStateRepository.make(db)
          service     <- MarketService.make[IO](repo, dispatcher)
          _           <- service.processSignals(Users.uid, Markets.gbpeur, List(Signals.trend(Direction.Downward, time = previousCandle)))
          _           <- service.processManualSignal(manualSignal)
          manualState <- repo.find(Users.uid, Markets.gbpeur)
          _           <- service.processSignals(Users.uid, Markets.gbpeur, List(candleSignal))
          state       <- repo.find(Users.uid, Markets.gbpeur)
        yield (manualState, state)

        result.map { case (manualState, state) =>
          manualState.flatMap(_.lastCandleTime) mustBe Some(previousCandle)
          state.flatMap(_.lastCandleTime) mustBe Some(completedCandle)
          state.flatMap(_.profile.lastClosePrice) mustBe Some(BigDecimal("1.25"))
          state.flatMap(_.profile.trend) mustBe Some(TrendState(Direction.Upward, manualSignal.time))
          dispatcher.submittedActions.toList mustBe List.fill(3)(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        }
      }

      "apply a weekend adjustment once across a downstream failure and accept signals from the same candle" in withEmbeddedMongoDb { db =>
        val dispatcher = MockActionDispatcher[IO]
        val reopenedAt = ts.plusSeconds(72 * 3600)
        val shiftedAt  = ts.plusSeconds(71 * 3600)
        val data       = weekendData(reopenedAt)
        val failure    = new RuntimeException("downstream signal processing failed")
        val result     = for
          repo <- MarketStateRepository.make(db)
          _ <- repo.save(initial.copy(profile = MarketProfile(trend = Some(TrendState(Direction.Upward, ts))), lastCandleTime = Some(ts)))
          _ <- repo.update(Users.uid, Markets.gbpeur, Some(PositionState(TradeOrder.Position.Buy, ts)))
          service       <- MarketService.make[IO](repo, dispatcher)
          firstAttempt  <- (service.updateTimeState(Users.uid, data) *> IO.raiseError[Unit](failure)).attempt
          firstShift    <- repo.find(Users.uid, Markets.gbpeur)
          _             <- service.updateTimeState(Users.uid, data)
          repeatedShift <- repo.find(Users.uid, Markets.gbpeur)
          _             <- service.processSignals(Users.uid, Markets.gbpeur, List(Signals.trend(Direction.Downward, time = reopenedAt)))
          state         <- repo.find(Users.uid, Markets.gbpeur)
        yield (firstAttempt, firstShift, repeatedShift, state)

        result.map { case (firstAttempt, firstShift, repeatedShift, state) =>
          firstAttempt mustBe Left(failure)
          repeatedShift mustBe firstShift
          firstShift.flatMap(_.profile.trend).map(_.confirmedAt) mustBe Some(shiftedAt)
          firstShift.flatMap(_.currentPosition).map(_.openedAt) mustBe Some(shiftedAt)
          state.flatMap(_.lastCandleTime) mustBe Some(reopenedAt)
          state.flatMap(_.profile.trend) mustBe Some(TrendState(Direction.Downward, reopenedAt))
          dispatcher.submittedActions.toList mustBe List(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        }
      }

      "record the first post-weekend candle before signals so its replay cannot shift newly created state" in withEmbeddedMongoDb { db =>
        val dispatcher = MockActionDispatcher[IO]
        val reopenedAt = ts.plusSeconds(72 * 3600)
        val data       = weekendData(reopenedAt)
        val result     = for
          repo          <- MarketStateRepository.make(db)
          service       <- MarketService.make[IO](repo, dispatcher)
          _             <- service.updateTimeState(Users.uid, data)
          _             <- service.processSignals(Users.uid, Markets.gbpeur, List(Signals.trend(Direction.Upward, time = reopenedAt)))
          signaledState <- repo.find(Users.uid, Markets.gbpeur)
          _             <- service.updateTimeState(Users.uid, data)
          state         <- repo.find(Users.uid, Markets.gbpeur)
        yield (signaledState, state)

        result.map { case (signaledState, state) =>
          state mustBe signaledState
          state.flatMap(_.profile.trend) mustBe Some(TrendState(Direction.Upward, reopenedAt))
        }
      }

      "recompute a candle from fresh state when an older candle commits between its read and write" in withEmbeddedMongoDb { db =>
        val olderTime = ts.plusSeconds(3600)
        val laterTime = ts.plusSeconds(7200)
        val older     = Signals.trend(Direction.Downward, time = olderTime)
        val later     = Signals.make(Users.uid, laterTime, Markets.gbpeur, Condition.ValueUpdated(ValueRole.Price, BigDecimal("1.25")))
        val result    = for
          realRepo <- MarketStateRepository.make(db)
          repo     <- interceptFirstSave(realRepo) {
            realRepo.find(Users.uid, Markets.gbpeur).flatMap { s =>
              realRepo.save(s.getOrElse(initial).applyCandleSignals(List(older), olderTime).get).void
            }
          }
          service <- MarketService.make[IO](repo, MockActionDispatcher[IO])
          _       <- service.processSignals(Users.uid, Markets.gbpeur, List(later))
          state   <- realRepo.find(Users.uid, Markets.gbpeur)
        yield state

        result.map { state =>
          state.flatMap(_.profile.trend) mustBe Some(TrendState(Direction.Downward, olderTime))
          state.flatMap(_.profile.lastClosePrice) mustBe Some(BigDecimal("1.25"))
          state.flatMap(_.lastCandleTime) mustBe Some(laterTime)
        }
      }

      "keep a position opened during a weekend adjustment and not shift it" in withEmbeddedMongoDb { db =>
        val reopenedAt  = ts.plusSeconds(72 * 3600)
        val replacement = PositionState(TradeOrder.Position.Sell, reopenedAt.plusSeconds(60), Some(BigDecimal("1.30")))
        val result      = for
          realRepo <- MarketStateRepository.make(db)
          _        <- realRepo.save(initial.copy(profile = MarketProfile(trend = Some(TrendState(Direction.Upward, ts)))))
          _        <- realRepo.update(Users.uid, Markets.gbpeur, Some(PositionState(TradeOrder.Position.Buy, ts)))
          repo     <- interceptFirstSave(realRepo)(realRepo.update(Users.uid, Markets.gbpeur, Some(replacement)).void)
          service  <- MarketService.make[IO](repo, MockActionDispatcher[IO])
          _        <- service.updateTimeState(Users.uid, weekendData(reopenedAt))
          state    <- realRepo.find(Users.uid, Markets.gbpeur)
        yield state

        result.map { state =>
          state.flatMap(_.currentPosition) mustBe Some(replacement)
          state.flatMap(_.profile.trend) mustBe Some(TrendState(Direction.Upward, ts.plusSeconds(71 * 3600)))
          state.flatMap(_.lastTimeStateCandle) mustBe Some(reopenedAt)
        }
      }
    }

    "update current position" should {
      "update position field in the state" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.save(initial.copy(profile = Markets.profile))
          res  <- repo.update(Users.uid, Markets.gbpeur, Some(Markets.positionState))
        yield res

        result.map { res =>
          res.currentPosition mustBe Some(Markets.positionState)
          res.profile mustBe Markets.profile
          res.version mustBe Some(2)
        }
      }
    }

    "find" should {
      "return empty option when state does not exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          res  <- repo.find(Users.uid, Markets.gbpusd)
        yield res

        result.map(_ mustBe None)
      }
    }

    "getAll" should {
      "return all market currency states" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.update(Users.uid, Markets.gbpeur, None)
          _    <- repo.update(Users.uid, Markets.gbpusd, None)
          res  <- repo.getAll(Users.uid)
        yield res

        result.map {
          _.map(_.withTime(ts)) mustBe List(
            MarketState(Users.uid, Markets.gbpeur, None, MarketProfile(), ts, ts, version = Some(1)),
            MarketState(Users.uid, Markets.gbpusd, None, MarketProfile(), ts, ts, version = Some(1))
          )
        }
      }
    }

    "deleteAll" should {
      "delete all market currency states" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.update(Users.uid, Markets.gbpeur, None)
          _    <- repo.update(Users.uid, Markets.gbpusd, None)
          _    <- repo.deleteAll(Users.uid)
          res  <- repo.getAll(Users.uid)
        yield res

        result.map(_ mustBe Nil)
      }
    }

    "delete" should {
      "delete market currency state" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.update(Users.uid, Markets.gbpeur, None)
          _    <- repo.delete(Users.uid, Markets.gbpeur)
          res  <- repo.find(Users.uid, Markets.gbpeur)
        yield res

        result.map(_ mustBe None)
      }

      "return error when market state does not exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.delete(Users.uid, Markets.gbpeur)
        yield ()

        result.attempt.map(_ mustBe Left(AppError.NotTracked(List(Markets.gbpeur))))
      }
    }
  }

  extension (s: MarketState) def withTime(ts: Instant): MarketState = s.copy(createdAt = ts, lastUpdatedAt = ts)

  private def storeLegacyState(db: MongoDatabase[IO], repo: MarketStateRepository[IO]): IO[MarketState] =
    for
      _          <- repo.update(Users.uid, Markets.gbpeur, None)
      collection <- db.getCollection(Repository.Collection.MarketState)
      _          <- collection.updateOne(Filter.eq("userId", Users.uid.toObjectId), Update.unset("version"))
      legacy     <- repo.find(Users.uid, Markets.gbpeur)
    yield legacy.get

  private def weekendData(reopenedAt: Instant) =
    Markets.timeSeriesData.copy(prices = NonEmptyList.of(Markets.priceRange.copy(time = reopenedAt), Markets.priceRange.copy(time = ts)))

  // Runs `concurrentWrite` just before the first save goes through, simulating another writer committing in between.
  private def interceptFirstSave(delegate: MarketStateRepository[IO])(concurrentWrite: IO[Unit]): IO[MarketStateRepository[IO]] =
    Ref.of[IO, Boolean](false).map { intercepted =>
      new MarketStateRepository[IO] {
        override def save(state: MarketState): IO[Boolean] =
          intercepted.getAndSet(true).flatMap(done => IO.unlessA(done)(concurrentWrite)) *> delegate.save(state)
        override def update(uid: UserId, pair: CurrencyPair, position: Option[PositionState]): IO[MarketState] =
          delegate.update(uid, pair, position)
        override def getAll(uid: UserId): IO[List[MarketState]]                   = delegate.getAll(uid)
        override def deleteAll(uid: UserId): IO[Unit]                             = delegate.deleteAll(uid)
        override def delete(uid: UserId, cp: CurrencyPair): IO[Unit]              = delegate.delete(uid, cp)
        override def find(uid: UserId, cp: CurrencyPair): IO[Option[MarketState]] = delegate.find(uid, cp)
      }
    }

  def withEmbeddedMongoDb[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo {
      MongoClient
        .fromConnectionString[IO](s"mongodb://localhost:$mongoPort")
        .use(_.getDatabase("currexx").flatMap(test))
    }.unsafeToFuture()(using IORuntime.global)
}
