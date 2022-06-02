package currexx.core.signal

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.core.{CatsSpec, FileReader}
import currexx.domain.user.UserId
import currexx.domain.market.{Condition, CurrencyPair, Indicator, MovingAverage, PriceRange, Trend, ValueSource, ValueTransformation}
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import io.circe.JsonObject

import java.time.LocalDate
import scala.collection.immutable.ListMap

class SignalServiceSpec extends CatsSpec {

  "A SignalService" when {
    "getSettings" should {
      "store signal-settings in the repository" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Signals.settings))

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
        when(settRepo.update(any[SignalSettings])).thenReturn(IO.unit)

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
        when(signRepo.save(any[Signal])).thenReturn(IO.unit)
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          _   <- svc.submit(Signals.trendDirectionChanged)
        yield ()

        result.asserting { res =>
          verify(signRepo).save(Signals.trendDirectionChanged)
          verify(disp).dispatch(Action.ProcessSignal(Signals.trendDirectionChanged))
          res mustBe ()
        }
      }
    }

    "getAll" should {
      "return all signals from the signRepository" in {
        val (signRepo, settRepo, disp) = mocks
        when(signRepo.getAll(any[UserId])).thenReturn(IO.pure(List(Signals.trendDirectionChanged)))

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.getAll(Users.uid)
        yield res

        result.asserting { res =>
          verifyNoInteractions(settRepo, disp)
          verify(signRepo).getAll(Users.uid)
          res mustBe List(Signals.trendDirectionChanged)
        }
      }
    }

    "processMarketData" should {
      "not do anything when there are no changes in market data since last point" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Signals.settings))

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
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Signals.settings))
        when(signRepo.isFirstOfItsKindForThatDate(any[Signal])).thenReturn(IO.pure(false))

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          val expectedSignal = Signal(
            Users.uid,
            Markets.gbpeur,
            Condition.TrendDirectionChange(Trend.Consolidation, Trend.Upward),
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
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Signals.settings.copy(triggerFrequency = TriggerFrequency.Continuously)))
        when(signRepo.save(any[Signal])).thenReturn(IO.unit)
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          val expectedSignal = Signal(
            Users.uid,
            Markets.gbpeur,
            Condition.TrendDirectionChange(Trend.Consolidation, Trend.Upward),
            Markets.trendChangeDetection,
            timeSeriesData.prices.head.time
          )
          verify(settRepo).get(Users.uid)
          verify(signRepo).save(expectedSignal)
          verify(disp).dispatch(Action.ProcessSignal(expectedSignal))
          verifyNoMoreInteractions(signRepo)
          res mustBe ()
        }
      }

      "not do anything when there are no changes in trend" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Signals.settings))
        when(signRepo.isFirstOfItsKindForThatDate(any[Signal])).thenReturn(IO.pure(false))

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
      "create signal when trend direction changes" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val indicator      = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.NMA(16, 8, 4.2d, MovingAverage.Weighted))
        val signal         = SignalService.detectTrendChange(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.TrendChangeDetection])

        val expectedCondition = Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation)
        signal mustBe Some(Signal(Users.uid, Markets.gbpeur, expectedCondition, indicator, timeSeriesData.prices.head.time))
      }

      "not do anything when trend hasn't changed" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
        val indicator      = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.NMA(16, 8, 4.2d, MovingAverage.Weighted))
        val signal         = SignalService.detectTrendChange(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.TrendChangeDetection])

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
