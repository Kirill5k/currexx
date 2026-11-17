package currexx.core.signal

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.core.MockActionDispatcher
import currexx.core.common.action.Action
import currexx.core.common.http.SearchParams
import currexx.core.fixtures.{Indicators, Markets, Processing, Settings, Signals, Users}
import currexx.core.settings.{GlobalSettings, SignalSettings, StrategyConfiguration}
import currexx.core.signal.db.SignalRepository
import currexx.domain.errors.AppError
import currexx.domain.signal.{Condition, Direction}
import currexx.domain.user.UserId
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import java.time.temporal.ChronoUnit

class SignalServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "A SignalService" when {
    "getAll" should {
      "return all signals from the signalRepository" in {
        val signal                     = Signals.trend(Direction.Upward)
        val (signRepo, settRepo, disp) = mocks
        when(signRepo.getAll(any[UserId], any[SearchParams]))
          .thenReturnIO(List(signal))

        val result = for
          svc <- SignalService
            .make[IO](signRepo, Processing.signals(settRepo), disp)
          res <- svc.getAll(Users.uid, SearchParams(Some(Signals.ts), None, Some(Markets.gbpeur)))
        yield res

        result.asserting { res =>
          verifyNoInteractions(settRepo)
          verify(signRepo).getAll(Users.uid, SearchParams(Some(Signals.ts), None, Some(Markets.gbpeur)))
          disp.submittedActions must be(empty)
          res mustBe List(signal)
        }
      }
    }

    "processMarketData" should {
      "raise NotSetup when signal settings are missing without dispatching a batch" in {
        given Clock[IO] = Clock.mock(Markets.timeSeriesData.latestTime)
        val signalRepo  = mock[SignalRepository[IO]]
        val dispatcher  = MockActionDispatcher[IO]
        val settings    = GlobalSettings(Users.uid, None, None, None)
        val runtime     = Processing.runtime(_ => IO.fromEither(StrategyConfiguration.from(settings)))

        val result = for
          svc <- SignalService.make[IO](signalRepo, runtime, dispatcher)
          res <- svc.processMarketData(Users.uid, Markets.timeSeriesData).attempt
        yield res

        result.asserting { res =>
          res mustBe Left(AppError.NotSetup("Signal"))
          verifyNoInteractions(signalRepo)
          dispatcher.submittedActions mustBe empty
        }
      }

      "dispatch an empty batch when signal settings explicitly contain no indicators" in {
        val data                       = Markets.timeSeriesData
        given Clock[IO]                = Clock.mock(data.latestTime)
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.apply(any[UserId])).thenReturnIO(SignalSettings(Nil))

        val result = for
          svc <- SignalService.make[IO](signRepo, Processing.signals(settRepo), disp)
          _   <- svc.processMarketData(Users.uid, data)
        yield ()

        result.asserting { _ =>
          verify(settRepo).apply(Users.uid)
          verifyNoInteractions(signRepo)
          disp.submittedActions mustBe List(
            Action.ProcessSignals(
              Users.uid,
              data.currencyPair,
              expectedBatch(data.latestTime).copy(sources = Map.empty, closureGap = SignalBatch.closureGap(data))
            )
          )
        }
      }

      "dispatch an empty batch when no indicator fires" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(3))

        given Clock[IO]                = Clock.mock(timeSeriesData.prices.head.time)
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.apply(any[UserId])).thenReturnIO(Settings.signal)

        val result = for
          svc <- SignalService
            .make[IO](signRepo, Processing.signals(settRepo), disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          verify(settRepo).apply(Users.uid)
          verifyNoInteractions(signRepo)
          disp.submittedActions mustBe List(
            Action.ProcessSignals(
              Users.uid,
              Markets.gbpeur,
              expectedBatch(timeSeriesData.latestTime).copy(closureGap = SignalBatch.closureGap(timeSeriesData))
            )
          )
          res mustBe ()
        }
      }

      "create signal when trend direction changes" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)

        given Clock[IO]                = Clock.mock(timeSeriesData.prices.head.time)
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.apply(any[UserId])).thenReturnIO(Settings.signal)
        when(signRepo.saveAll(anyList[Signal])).thenReturnUnit

        val result = for
          svc <- SignalService
            .make[IO](signRepo, Processing.signals(settRepo), disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          val expectedSignal = Signal(
            userId = Users.uid,
            interval = Markets.timeSeriesData.interval,
            currencyPair = Markets.gbpeur,
            condition = Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(13)),
            triggeredBy = Indicators.trendChangeDetection,
            time = timeSeriesData.prices.head.time
          )

          verify(settRepo).apply(Users.uid)
          verify(signRepo).saveAll(List(expectedSignal))
          verifyNoMoreInteractions(signRepo)
          disp.submittedActions mustBe List(
            Action.ProcessSignals(
              Users.uid,
              Markets.gbpeur,
              expectedBatch(expectedSignal.time, List(expectedSignal)).copy(closureGap = SignalBatch.closureGap(timeSeriesData))
            )
          )
          res mustBe ()
        }
      }

      "dispatch an empty batch when trend is unchanged" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))

        given Clock[IO]                = Clock.mock(timeSeriesData.prices.head.time)
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.apply(any[UserId])).thenReturnIO(Settings.signal)

        val result = for
          svc <- SignalService
            .make[IO](signRepo, Processing.signals(settRepo), disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          verify(settRepo).apply(Users.uid)
          verifyNoInteractions(signRepo)
          disp.submittedActions mustBe List(
            Action.ProcessSignals(
              Users.uid,
              Markets.gbpeur,
              expectedBatch(timeSeriesData.latestTime).copy(closureGap = SignalBatch.closureGap(timeSeriesData))
            )
          )
          res mustBe ()
        }
      }

      "dispatch an empty batch for stale data" in {
        val timeSeriesData = Markets.timeSeriesData

        given Clock[IO] = Clock.mock(timeSeriesData.prices.head.time.plus(2, ChronoUnit.DAYS))

        val (signRepo, settRepo, disp) = mocks
        when(settRepo.apply(any[UserId])).thenReturnIO(Settings.signal)

        val result = for
          svc <- SignalService
            .make[IO](signRepo, Processing.signals(settRepo), disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          verifyNoInteractions(signRepo)
          verify(settRepo).apply(Users.uid)
          disp.submittedActions mustBe List(
            Action.ProcessSignals(
              Users.uid,
              Markets.gbpeur,
              expectedBatch(timeSeriesData.latestTime).copy(closureGap = SignalBatch.closureGap(timeSeriesData))
            )
          )
          res mustBe ()
        }
      }
    }
  }

  private def expectedBatch(time: Instant, signals: List[Signal] = Nil): SignalBatch =
    SignalBatch(
      interval = Markets.timeSeriesData.interval,
      revision = 0L,
      candleTime = time,
      closureGap = None,
      sources = Map(Indicators.trendChangeDetection.resolvedId.get -> Indicators.trendChangeDetection),
      signals = signals
    )

  def mocks: (SignalRepository[IO], UserId => IO[SignalSettings], MockActionDispatcher[IO]) =
    (mock[SignalRepository[IO]], mock[UserId => IO[SignalSettings]], MockActionDispatcher[IO])

  extension [A](nel: NonEmptyList[A])
    def drop(n: Int): NonEmptyList[A] =
      NonEmptyList.fromListUnsafe(nel.toList.drop(n))
}
