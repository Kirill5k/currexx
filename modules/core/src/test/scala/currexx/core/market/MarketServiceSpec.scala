package currexx.core.market

import cats.MonadThrow
import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import currexx.core.MockActionDispatcher
import currexx.core.common.action.Action
import currexx.core.fixtures.Processing
import currexx.core.fixtures.{Markets, Settings, Signals, Trades, Users}
import currexx.core.market.db.MarketStateRepository
import currexx.core.signal.SignalBatch
import currexx.core.settings.{StrategyConfiguration, StrategyRuntime}
import currexx.core.settings.db.SettingsRepository
import currexx.core.trade.BrokerPosition
import currexx.domain.market.{CurrencyPair, Interval, OrderExecution, TradeOrder}
import currexx.domain.signal.{Condition, Direction, ValueRole}
import currexx.domain.user.UserId
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import kirill5k.common.syntax.time.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

class MarketServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]
  given Clock[IO]  = Clock.mock[IO](Markets.ts)

  "A MarketService" when {
    "clearState" should {
      "delete all existing market states and close orders" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.deleteAll(any[UserId])).thenReturnUnit

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          state <- svc.clearState(Users.uid, true)
        yield state

        result.asserting { res =>
          verify(stateRepo).deleteAll(Users.uid)
          disp.submittedActions mustBe List(Action.CloseAllOpenOrders(Users.uid))
          res mustBe ()
        }
      }

      "delete all existing market states without closing orders" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.deleteAll(any[UserId])).thenReturnUnit

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          state <- svc.clearState(Users.uid, false)
        yield state

        result.asserting { res =>
          verify(stateRepo).deleteAll(Users.uid)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "delete existing market for a single currency" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.delete(any[UserId], any[CurrencyPair])).thenReturnUnit

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          state <- svc.clearState(Users.uid, Markets.gbpeur, true)
        yield state

        result.asserting { res =>
          verify(stateRepo).delete(Users.uid, Markets.gbpeur)
          disp.submittedActions mustBe List(Action.CloseOpenOrders(Users.uid, Markets.gbpeur))
          res mustBe ()
        }
      }
    }

    "getState" should {
      "return state of all traded currencies" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.getAll(any[UserId])).thenReturnIO(List(Markets.state))

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          state <- svc.getState(Users.uid)
        yield state

        result.asserting { res =>
          verify(stateRepo).getAll(Users.uid)
          disp.submittedActions mustBe empty
          res mustBe List(Markets.state)
        }
      }
    }

    "processTradeOrderPlacement" should {
      val openedAt  = Markets.ts.minusSeconds(600)
      val filledAt  = Trades.execution.time
      val longAt    = (price: String) => Some(PositionState(TradeOrder.Position.Buy, openedAt, Some(BigDecimal(price))))
      val shortAt   = (price: String) => Some(PositionState(TradeOrder.Position.Sell, openedAt, Some(BigDecimal(price))))
      val long      = (price: String) => Some(BrokerPosition.Open(TradeOrder.Position.Buy, BigDecimal(price)))
      val short     = (price: String) => Some(BrokerPosition.Open(TradeOrder.Position.Sell, BigDecimal(price)))
      val execution = List(Trades.execution)

      def placeEntry(current: Option[PositionState], executions: List[OrderExecution], brokerPosition: Option[BrokerPosition]) = {
        val (stateRepo, disp) = mocks
        val state             = Markets.state.copy(currentPosition = current)
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(state)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          _   <- svc.processTradeOrderPlacement(Trades.order.copy(executions = executions, brokerPosition = brokerPosition))
        yield ()
        result.map { _ =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          disp.submittedActions mustBe empty
          (stateRepo, state)
        }
      }

      List[(String, Option[PositionState], List[OrderExecution], Option[BrokerPosition], Option[PositionState])](
        (
          "open the position at the broker's entry price and the time it filled",
          None,
          execution,
          long("1.15"),
          Some(PositionState(TradeOrder.Position.Buy, filledAt, Some(BigDecimal("1.15"))))
        ),
        (
          "open a position without an entry price when the broker's position is unknown after the fill",
          None,
          execution,
          Some(BrokerPosition.Unknown),
          Some(PositionState(TradeOrder.Position.Buy, filledAt))
        ),
        (
          "open a position without an entry price while its entry is pending",
          None,
          Nil,
          None,
          Some(PositionState(TradeOrder.Position.Buy, Trades.ts))
        ),
        (
          "take the broker's entry price after adding to a position, keeping when it opened",
          longAt("1.10"),
          execution,
          long("1.15"),
          longAt("1.15")
        ),
        (
          "drop the entry price after adding to a position when the broker's position is unknown",
          longAt("1.10"),
          Nil,
          Some(BrokerPosition.Unknown),
          Some(PositionState(TradeOrder.Position.Buy, openedAt))
        ),
        (
          "replace a position on the opposite side",
          shortAt("1.10"),
          execution,
          long("1.15"),
          Some(PositionState(TradeOrder.Position.Buy, filledAt, Some(BigDecimal("1.15"))))
        ),
        ("clear a position that the entry closed at the broker", shortAt("1.10"), execution, Some(BrokerPosition.Flat), None)
      ).foreach { case (description, current, executions, brokerPosition, expected) =>
        description in placeEntry(current, executions, brokerPosition).asserting { (stateRepo, state) =>
          verify(stateRepo).save(state.copy(currentPosition = expected))
          succeed
        }
      }

      List[(String, Option[PositionState], List[OrderExecution], Option[BrokerPosition])](
        ("a replayed fill", longAt("1.15"), execution, long("1.15")),
        ("an entry that only reduces the opposite side at the broker", shortAt("1.10"), execution, short("1.10")),
        ("an entry on the same side that is pending", longAt("1.10"), Nil, None)
      ).foreach { case (description, current, executions, brokerPosition) =>
        s"leave the position unchanged by $description" in placeEntry(current, executions, brokerPosition).asserting { (stateRepo, _) =>
          verifyNoMoreInteractions(stateRepo)
          succeed
        }
      }

      "clear the position on exit" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[Option[PositionState]])).thenReturn(IO.unit)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          state <- svc.processTradeOrderPlacement(Trades.order.copy(order = TradeOrder.Exit(Markets.gbpeur, 1.3)))
        yield state

        result.asserting { res =>
          verify(stateRepo).update(Users.uid, Markets.gbpeur, None)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }

    "reconcileEntryPrice" should {
      val unpriced = Markets.state.copy(
        currentPosition = Some(PositionState(TradeOrder.Position.Buy, Markets.ts)),
        version = Some(1),
        configRevision = Some(0L),
        sources = Processing.sources
      )
      val price    = BigDecimal("1.1002")
      val repaired = unpriced.copy(currentPosition = Some(PositionState(TradeOrder.Position.Buy, Markets.ts, Some(price))))

      "save the broker's entry price against the state read before the broker" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          state <- svc.reconcileEntryPrice(unpriced, BrokerPosition.Open(TradeOrder.Position.Buy, price))
        yield state

        result.asserting { res =>
          verify(stateRepo).save(repaired)
          verifyNoMoreInteractions(stateRepo)
          disp.submittedActions mustBe empty
          res mustBe repaired
        }
      }

      "repair a missing price without loading or validating strategy configuration" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)
        val invalid = Processing.runtime(_ => IO.raiseError(new IllegalStateException("invalid strategy")))
        (for
          service <- MarketService.make[IO](stateRepo, disp, invalid)
          result  <- service.reconcileEntryPrice(unpriced, BrokerPosition.Open(TradeOrder.Position.Buy, price))
        yield result).asserting { result =>
          result mustBe repaired
          verify(stateRepo).save(repaired)
          verifyNoMoreInteractions(stateRepo)
          disp.submittedActions mustBe empty
        }
      }

      List[(String, MarketState, BrokerPosition)](
        ("the broker's position is flat", unpriced, BrokerPosition.Flat),
        ("the broker's position is unknown", unpriced, BrokerPosition.Unknown),
        ("the broker's position is on the opposite side", unpriced, BrokerPosition.Open(TradeOrder.Position.Sell, price)),
        (
          "the entry price is already known",
          unpriced.copy(currentPosition = Some(PositionState(TradeOrder.Position.Buy, Markets.ts, Some(BigDecimal("1.2"))))),
          BrokerPosition.Open(TradeOrder.Position.Buy, price)
        ),
        ("no position is held", unpriced.copy(currentPosition = None), BrokerPosition.Open(TradeOrder.Position.Buy, price))
      ).foreach { case (description, state, brokerPosition) =>
        s"return the state as it was read when $description" in {
          val (stateRepo, disp) = mocks

          val result = for
            svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
            res <- svc.reconcileEntryPrice(state, brokerPosition)
          yield res

          result.asserting { res =>
            verifyNoInteractions(stateRepo)
            disp.submittedActions mustBe empty
            res mustBe state
          }
        }
      }

      List[(String, MarketState)](
        ("closed", unpriced.copy(currentPosition = None, version = Some(2))),
        (
          "replaced by another same-side position without a price",
          unpriced.copy(currentPosition = Some(PositionState(TradeOrder.Position.Buy, Markets.ts.plusSeconds(60))), version = Some(3))
        )
      ).foreach { case (description, latest) =>
        s"discard the broker's price when the position was $description while it was being read" in {
          val (stateRepo, disp) = mocks
          when(stateRepo.save(any[MarketState])).thenReturnIO(false)
          when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(latest)

          val result = for
            svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
            res <- svc.reconcileEntryPrice(unpriced, BrokerPosition.Open(TradeOrder.Position.Buy, price))
          yield res

          result.asserting { res =>
            verify(stateRepo).save(repaired)
            verify(stateRepo).find(Users.uid, Markets.gbpeur)
            verifyNoMoreInteractions(stateRepo)
            disp.submittedActions mustBe empty
            res mustBe latest
          }
        }
      }
    }

    "processSignals" should {
      "reject batches from inactive configurations before saving state" in {
        val settings = mock[SettingsRepository[IO]]
        val states   = mock[MarketStateRepository[IO]]
        val config   = StrategyConfiguration.from(Settings.global.copy(revision = 2L)).toOption.get
        when(settings.get(Users.uid)).thenReturnIO(Settings.global.copy(revision = 2L))
        val batch    = Processing.batch(Nil).copy(revision = 2L, sources = config.sources, closureGap = Some(71.hours))
        val obsolete = List(batch.copy(revision = 1L), batch.copy(sources = Map.empty))
        when(states.find(Users.uid, Markets.gbpeur)).thenReturnSome(Markets.state)
        val dispatcher = MockActionDispatcher[IO]
        (for
          service <- MarketService.make[IO](states, dispatcher, StrategyRuntime.make(settings))
          _       <- obsolete.traverse_(service.processSignals(Users.uid, Markets.gbpeur, _))
        yield ()).asserting { _ =>
          verify(states, times(obsolete.size)).find(Users.uid, Markets.gbpeur)
          verifyNoMoreInteractions(states)
          dispatcher.submittedActions mustBe empty
        }
      }

      "recheck the configuration after an optimistic write conflict" in {
        val states  = mock[MarketStateRepository[IO]]
        val runtime = mock[StrategyRuntime[IO]]
        val batch   = Processing.batch(List(Signals.trend(Direction.Downward)))
        when(states.find(Users.uid, Markets.gbpeur)).thenReturnSome(Markets.state)
        when(runtime.load(Users.uid)).thenReturn(
          Processing.fixed.load(Users.uid),
          Processing.fixed.load(Users.uid).map(_.copy(revision = 1L))
        )
        when(states.save(any[MarketState])).thenReturnIO(false)
        val dispatcher = MockActionDispatcher[IO]
        (for
          service <- MarketService.make[IO](states, dispatcher, runtime)
          _       <- service.processSignals(Users.uid, Markets.gbpeur, batch)
        yield ()).asserting { _ =>
          verify(states, times(2)).find(Users.uid, Markets.gbpeur)
          verify(states).save(any[MarketState])
          verify(runtime, times(2)).load(Users.uid)
          dispatcher.submittedActions mustBe empty
        }
      }

      "save the updated state and evaluate rules when the profile changes" in {
        val signal            = Signals.trend(Direction.Downward)
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(configuredState)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, Processing.batch(List(signal)))
        yield ()

        result.asserting { _ =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).save(
            configuredState.copy(
              profile = MarketProfileUpdater.update(configuredState.profile, signal),
              previousProfile = Some(configuredState.profile),
              lastCandleTime = Some(signal.time),
              lastTimeStateCandle = Some(signal.time)
            )
          )
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        }
      }

      "save the candle watermark without evaluating rules when the profile is unchanged" in {
        val signal            = Signals.trend(Direction.Upward)
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(configuredState)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, Processing.batch(List(signal)))
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(configuredState.copy(lastCandleTime = Some(signal.time), lastTimeStateCandle = Some(signal.time)))
          disp.submittedActions mustBe empty
        }
      }

      "skip older and repeated candles without saving" in {
        val candleTime        = Markets.ts.plus(2.hours)
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(configuredState.copy(lastCandleTime = Some(candleTime)))

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          _   <- svc.processSignals(
            Users.uid,
            Markets.gbpeur,
            Processing.batch(List(Signals.trend(Direction.Downward, time = candleTime.minus(1.hour))))
          )
          _ <- svc.processSignals(Users.uid, Markets.gbpeur, Processing.batch(List(Signals.trend(Direction.Downward, time = candleTime))))
        yield ()

        result.asserting { _ =>
          verify(stateRepo, times(2)).find(Users.uid, Markets.gbpeur)
          verifyNoMoreInteractions(stateRepo)
          disp.submittedActions mustBe empty
        }
      }

      "create the state when it does not exist yet" in {
        val signal            = Signals.trend(Direction.Downward)
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnNone
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, Processing.batch(List(signal)))
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(
            MarketState
              .initial(Users.uid, Markets.gbpeur, Markets.ts)
              .copy(
                profile = MarketProfileUpdater.update(MarketProfile(), signal),
                previousProfile = Some(MarketProfile()),
                configRevision = Some(0L),
                sources = Processing.sources,
                interval = Some(Interval.H1),
                lastCandleTime = Some(signal.time),
                lastTimeStateCandle = Some(signal.time)
              )
          )
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        }
      }

      "reload and recompute from the latest state after a version conflict" in {
        val olderTime  = Markets.ts.plus(1.hour)
        val candleTime = Markets.ts.plus(2.hours)
        val committed  =
          configuredState.copy(
            profile = MarketProfileUpdater.update(configuredState.profile, Signals.trend(Direction.Downward, time = olderTime)),
            version = Some(1)
          )
        val signal = Signals.make(Users.uid, candleTime, Markets.gbpeur, Condition.ValueUpdated(ValueRole.Price, BigDecimal("1.25")))
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturn(IO.pure(Some(configuredState)), IO.pure(Some(committed)))
        when(stateRepo.save(any[MarketState])).thenReturn(IO.pure(false), IO.pure(true))

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, Processing.batch(List(signal)))
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(
            configuredState.copy(
              profile = MarketProfileUpdater.update(configuredState.profile, signal),
              previousProfile = Some(configuredState.profile),
              lastCandleTime = Some(candleTime),
              lastTimeStateCandle = Some(candleTime)
            )
          )
          verify(stateRepo).save(
            committed.copy(
              profile = MarketProfileUpdater.update(committed.profile, signal),
              previousProfile = Some(committed.profile),
              lastCandleTime = Some(candleTime),
              lastTimeStateCandle = Some(candleTime)
            )
          )
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        }
      }

      "stop after a bounded number of conflicting attempts" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(configuredState)
        when(stateRepo.save(any[MarketState])).thenReturnIO(false)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          res <- svc.processSignals(Users.uid, Markets.gbpeur, Processing.batch(List(Signals.trend(Direction.Downward)))).attempt
        yield res

        result.asserting { res =>
          res.left.map(_.getClass) mustBe Left(classOf[IllegalStateException])
          verify(stateRepo, times(5)).find(Users.uid, Markets.gbpeur)
          verify(stateRepo, times(5)).save(any[MarketState])
          disp.submittedActions mustBe empty
        }
      }

      "log the configuration guard's reason even when the rejected batch is empty" in {
        val underlying        = summon[Logger[IO]]
        val (stateRepo, disp) = mocks
        when(stateRepo.find(Users.uid, Markets.gbpeur)).thenReturnSome(configuredState)
        (for
          messages <- Ref.of[IO, List[String]](Nil)
          logger = new Logger[IO]:
            export underlying.{debug, error, trace, warn}
            def info(message: => String): IO[Unit]               = messages.update(_ :+ message)
            def info(t: Throwable)(message: => String): IO[Unit] = info(message)
          service <- MarketService.make[IO](stateRepo, disp, Processing.fixed)(using
            MonadThrow[IO],
            summon[Clock[IO]],
            logger
          )
          _      <- service.processSignals(Users.uid, Markets.gbpeur, Processing.batch(Nil).copy(revision = 1L))
          logged <- messages.get
        yield logged).asserting { messages =>
          messages mustBe List(
            s"rejecting signal batch for ${Users.uid}/${Markets.gbpeur} at ${Markets.ts}: " +
              "configuration revision 1 is no longer active (0)"
          )
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verifyNoMoreInteractions(stateRepo)
          disp.submittedActions mustBe empty
        }
      }

      "advance only the time cursor for empty batches without evaluating rules" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(configuredState)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)
        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, Processing.batch(Nil))
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(configuredState.copy(lastTimeStateCandle = Some(Markets.ts)))
          disp.submittedActions mustBe empty
        }
      }
    }

    "closure adjustment in a batch" should {
      val candleTime  = Markets.ts.plus(1.day)
      val dataWithGap = Markets.timeSeriesData.copy(prices = Markets.priceRanges.head.copy(time = candleTime) :: Markets.priceRanges)

      "shift the state when the gap between the latest two candles is greater than two intervals" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(configuredState)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          _   <- svc.processSignals(
            Users.uid,
            dataWithGap.currencyPair,
            Processing
              .batch(Nil)
              .copy(interval = dataWithGap.interval, candleTime = dataWithGap.latestTime, closureGap = SignalBatch.closureGap(dataWithGap))
          )
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(
            configuredState
              .adjustForMarketClosure(1.day - 1.hour, candleTime)
              .copy(lastTimeStateCandle = Some(candleTime))
          )
          disp.submittedActions mustBe empty
        }
      }

      "only record the candle when there is no significant gap" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(configuredState)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          _   <- svc.processSignals(
            Users.uid,
            Markets.timeSeriesData.currencyPair,
            Processing
              .batch(Nil)
              .copy(
                interval = Markets.timeSeriesData.interval,
                candleTime = Markets.timeSeriesData.latestTime,
                closureGap = SignalBatch.closureGap(Markets.timeSeriesData)
              )
          )
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(
            configuredState.copy(
              lastTimeStateCandle = Some(Markets.timeSeriesData.latestTime)
            )
          )
          disp.submittedActions mustBe empty
        }
      }

      "skip a gap that was already applied" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair]))
          .thenReturnSome(configuredState.copy(lastCandleTime = Some(candleTime), lastTimeStateCandle = Some(candleTime)))

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp, Processing.fixed)
          _   <- svc.processSignals(
            Users.uid,
            dataWithGap.currencyPair,
            Processing
              .batch(Nil)
              .copy(interval = dataWithGap.interval, candleTime = dataWithGap.latestTime, closureGap = SignalBatch.closureGap(dataWithGap))
          )
        yield ()

        result.asserting { _ =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verifyNoMoreInteractions(stateRepo)
          disp.submittedActions mustBe empty
        }
      }
    }
  }

  private val configuredState = Markets.state.copy(
    configRevision = Some(0L),
    interval = Some(Interval.H1),
    sources = Processing.sources
  )

  def mocks: (MarketStateRepository[IO], MockActionDispatcher[IO]) =
    (mock[MarketStateRepository[IO]], MockActionDispatcher[IO])
}
