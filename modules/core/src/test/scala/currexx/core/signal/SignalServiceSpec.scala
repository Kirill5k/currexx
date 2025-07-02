package currexx.core.signal

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.core.MockActionDispatcher
import currexx.core.common.action.Action
import currexx.core.common.http.SearchParams
import currexx.core.fixtures.{Markets, Settings, Signals, Users}
import currexx.core.settings.TriggerFrequency
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.domain.market.{Condition, Direction, Indicator, ValueSource, ValueTransformation as VT}
import currexx.domain.user.UserId
import kirill5k.common.cats.test.IOWordSpec

class SignalServiceSpec extends IOWordSpec {

  "A SignalService" when {
    "submit" should {
      "store new signal in the repository and dispatch an action" in {
        val (signRepo, settRepo, disp) = mocks
        when(signRepo.saveAll(anyList[Signal])).thenReturnUnit

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          _   <- svc.submit(Signals.trendDirectionChanged)
        yield ()

        result.asserting { res =>
          verify(signRepo).saveAll(List(Signals.trendDirectionChanged))
          disp.submittedActions mustBe List(Action.ProcessSignals(Users.uid, Markets.gbpeur, List(Signals.trendDirectionChanged)))
          res mustBe ()
        }
      }
    }

    "getAll" should {
      "return all signals from the signalRepository" in {
        val (signRepo, settRepo, disp) = mocks
        when(signRepo.getAll(any[UserId], any[SearchParams])).thenReturnIO(List(Signals.trendDirectionChanged))

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.getAll(Users.uid, SearchParams(Some(Signals.ts), None, Some(Markets.gbpeur)))
        yield res

        result.asserting { res =>
          verifyNoInteractions(settRepo)
          verify(signRepo).getAll(Users.uid, SearchParams(Some(Signals.ts), None, Some(Markets.gbpeur)))
          disp.submittedActions mustBe empty
          res mustBe List(Signals.trendDirectionChanged)
        }
      }
    }

    "processMarketData" should {
      "not do anything when there are no changes in market data since last point" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.signal)

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(3)))
        yield res

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(signRepo)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "not submit a signal if such signal has already been submitted on that date" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.signal)
        when(signRepo.isFirstOfItsKindForThatDate(any[Signal])).thenReturnIO(false)

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val result         = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          val expectedSignal = Signal(
            userId = Users.uid,
            interval = Markets.timeSeriesData.interval,
            currencyPair = Markets.gbpeur,
            condition = Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(13)),
            triggeredBy = Markets.trendChangeDetection,
            time = timeSeriesData.prices.head.time
          )
          verify(settRepo).get(Users.uid)
          verify(signRepo).isFirstOfItsKindForThatDate(expectedSignal)
          verifyNoMoreInteractions(signRepo)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "create signal when trend direction changes" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.signal.copy(triggerFrequency = TriggerFrequency.Continuously))
        when(signRepo.saveAll(anyList[Signal])).thenReturnUnit

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val result         = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          val expectedSignal = Signal(
            userId = Users.uid,
            interval = Markets.timeSeriesData.interval,
            currencyPair = Markets.gbpeur,
            condition = Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(13)),
            triggeredBy = Markets.trendChangeDetection,
            time = timeSeriesData.prices.head.time
          )

          verify(settRepo).get(Users.uid)
          verify(signRepo).saveAll(List(expectedSignal))
          verifyNoMoreInteractions(signRepo)
          disp.submittedActions mustBe List(Action.ProcessSignals(Users.uid, Markets.gbpeur, List(expectedSignal)))
          res mustBe ()
        }
      }

      "not do anything when there are no changes in trend" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.signal)
        when(signRepo.isFirstOfItsKindForThatDate(any[Signal])).thenReturnIO(false)

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
        val result         = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoMoreInteractions(signRepo)
          verifyNoInteractions(signRepo)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }

    "detectTrendChange" should {
      val indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.HMA(16))

      "create signal when trend direction changes" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val signal = SignalService.detectTrendChange(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.TrendChangeDetection])

        signal mustBe Some(
          Signal(
            userId = Users.uid,
            interval = Markets.timeSeriesData.interval,
            currencyPair = Markets.gbpeur,
            condition = Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(13)),
            triggeredBy = indicator,
            time = timeSeriesData.prices.head.time
          )
        )
      }

      "not do anything when trend hasn't changed" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
        val signal = SignalService.detectTrendChange(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.TrendChangeDetection])

        signal mustBe None
      }
    }

    "detectThresholdCrossing" should {
      val indicator = Indicator.ThresholdCrossing(ValueSource.Close, VT.STOCH(14), 80d, 20d)
      "create signal when current value is below threshold" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val signal = SignalService.detectThresholdCrossing(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.ThresholdCrossing])

        signal mustBe Some(
          Signal(
            userId = Users.uid,
            interval = Markets.timeSeriesData.interval,
            currencyPair = Markets.gbpeur,
            condition = Condition.BelowThreshold(20d, BigDecimal(16.294773928361835)),
            triggeredBy = indicator,
            time = timeSeriesData.prices.head.time
          )
        )
      }

      "not do anything when current value is within limits" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(10))
        val signal = SignalService.detectThresholdCrossing(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.ThresholdCrossing])

        signal mustBe None
      }
    }

    "detectSignal" should {
      val indicator = Indicator.Composite(
        NonEmptyList.of(
          Indicator.ThresholdCrossing(ValueSource.Close, VT.STOCH(14), 80d, 20d),
          Indicator.TrendChangeDetection(ValueSource.Close, VT.HMA(16))
        )
      )

      "return composite condition when all indicators generate signals" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)

        val signal = SignalService.detectSignal(Users.uid, timeSeriesData, indicator)

        signal mustBe Some(
          Signal(
            userId = Users.uid,
            currencyPair = Markets.gbpeur,
            interval = Markets.timeSeriesData.interval,
            condition = Condition.Composite(
              NonEmptyList.of(
                Condition.BelowThreshold(20d, BigDecimal(16.294773928361835)),
                Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(13))
              )
            ),
            triggeredBy = indicator,
            time = timeSeriesData.prices.head.time
          )
        )
      }
      "not return anything when only one indicator generated signal" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))

        val signal = SignalService.detectSignal(Users.uid, timeSeriesData, indicator)

        signal mustBe None
      }
    }
  }

  def mocks: (SignalRepository[IO], SignalSettingsRepository[IO], MockActionDispatcher[IO]) =
    (mock[SignalRepository[IO]], mock[SignalSettingsRepository[IO]], MockActionDispatcher[IO])

  extension [A](nel: NonEmptyList[A])
    def drop(n: Int): NonEmptyList[A] =
      NonEmptyList.fromListUnsafe(nel.toList.drop(n))
}
