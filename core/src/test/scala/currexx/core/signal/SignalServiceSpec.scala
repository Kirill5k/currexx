package currexx.core.signal

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.core.{IOWordSpec, FileReader}
import currexx.domain.user.UserId
import currexx.domain.market.{Condition, CurrencyPair, Indicator, MovingAverage, PriceRange, Trend, ValueSource, ValueTransformation as VT}
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import io.circe.JsonObject

import java.time.{Instant, LocalDate}
import scala.collection.immutable.ListMap

class SignalServiceSpec extends IOWordSpec {

  "A SignalService" when {
    "getSettings" should {
      "store signal-settings in the repository" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Signals.settings)

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          _   <- svc.getSettings(Users.uid)
        yield ()

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(signRepo, disp)
          res mustBe ()
        }
      }
    }

    "updateSettings" should {
      "store signal-settings in the repository" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.update(any[SignalSettings])).thenReturnUnit

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          _   <- svc.updateSettings(Signals.settings)
        yield ()

        result.asserting { res =>
          verify(settRepo).update(Signals.settings)
          verifyNoInteractions(signRepo, disp)
          res mustBe ()
        }
      }
    }

    "submit" should {
      "store new signal in the repository and dispatch an action" in {
        val (signRepo, settRepo, disp) = mocks
        when(signRepo.saveAll(anyList[Signal])).thenReturnUnit
        when(disp.dispatch(any[Action])).thenReturnUnit

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          _   <- svc.submit(Signals.trendDirectionChanged)
        yield ()

        result.asserting { res =>
          verify(signRepo).saveAll(List(Signals.trendDirectionChanged))
          verify(disp).dispatch(Action.ProcessSignals(Users.uid, Markets.gbpeur, List(Signals.trendDirectionChanged)))
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
          verifyNoInteractions(settRepo, disp)
          verify(signRepo).getAll(Users.uid, SearchParams(Some(Signals.ts), None, Some(Markets.gbpeur)))
          res mustBe List(Signals.trendDirectionChanged)
        }
      }
    }

    "processMarketData" should {
      "not do anything when there are no changes in market data since last point" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Signals.settings)

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(3)))
        yield res

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(disp, signRepo)
          res mustBe ()
        }
      }

      "not submit a signal if such signal has already been submitted on that date" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Signals.settings)
        when(signRepo.isFirstOfItsKindForThatDate(any[Signal])).thenReturnIO(false)

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          val expectedSignal = Signal(
            Users.uid,
            Markets.gbpeur,
            Condition.TrendDirectionChange(Trend.Consolidation, Trend.Upward, Some(1)),
            Markets.trendChangeDetection,
            timeSeriesData.prices.head.time
          )
          verify(settRepo).get(Users.uid)
          verify(signRepo).isFirstOfItsKindForThatDate(expectedSignal)
          verifyNoMoreInteractions(signRepo)
          verifyNoInteractions(disp)
          res mustBe ()
        }
      }

      "create signal when trend direction changes" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Signals.settings.copy(triggerFrequency = TriggerFrequency.Continuously))
        when(signRepo.saveAll(anyList[Signal])).thenReturnUnit
        when(disp.dispatch(any[Action])).thenReturnUnit

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          val expectedSignal = Signal(
            Users.uid,
            Markets.gbpeur,
            Condition.TrendDirectionChange(Trend.Consolidation, Trend.Upward, Some(1)),
            Markets.trendChangeDetection,
            timeSeriesData.prices.head.time
          )
          verify(settRepo).get(Users.uid)
          verify(signRepo).saveAll(List(expectedSignal))
          verify(disp).dispatch(Action.ProcessSignals(Users.uid, Markets.gbpeur, List(expectedSignal)))
          verifyNoMoreInteractions(signRepo)
          res mustBe ()
        }
      }

      "not do anything when there are no changes in trend" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Signals.settings)
        when(signRepo.isFirstOfItsKindForThatDate(any[Signal])).thenReturnIO(false)

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoMoreInteractions(signRepo)
          verifyNoInteractions(disp, signRepo)
          res mustBe ()
        }
      }
    }

    "detectTrendChange" should {
      val indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.SingleOutput.NMA(16, 8, 4.2d, MovingAverage.Weighted))

      "create signal when trend direction changes" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val signal = SignalService.detectTrendChange(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.TrendChangeDetection])

        val expectedCondition = Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation, Some(1))
        signal mustBe Some(Signal(Users.uid, Markets.gbpeur, expectedCondition, indicator, timeSeriesData.prices.head.time))
      }

      "not do anything when trend hasn't changed" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
        val signal = SignalService.detectTrendChange(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.TrendChangeDetection])

        signal mustBe None
      }
    }

    "detectThresholdCrossing" should {
      val indicator     = Indicator.ThresholdCrossing(ValueSource.Close, VT.DoubleOutput.STOCH(14, 3, 3), 80D, 20D)
      "create signal when current value is below threshold" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val signal = SignalService.detectThresholdCrossing(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.ThresholdCrossing])

        val expectedCondition = Condition.BelowThreshold(20D, BigDecimal(18.94698816942126))
        signal mustBe Some(Signal(Users.uid, Markets.gbpeur, expectedCondition, indicator, timeSeriesData.prices.head.time))
      }

      "not do anything when current value is within limits" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(10))
        val signal = SignalService.detectThresholdCrossing(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.ThresholdCrossing])

        signal mustBe None
      }
    }
  }

  def mocks: (SignalRepository[IO], SignalSettingsRepository[IO], ActionDispatcher[IO]) =
    (mock[SignalRepository[IO]], mock[SignalSettingsRepository[IO]], mock[ActionDispatcher[IO]])

  extension [A](nel: NonEmptyList[A])
    def drop(n: Int): NonEmptyList[A] =
      NonEmptyList.fromListUnsafe(nel.toList.drop(n))
}
