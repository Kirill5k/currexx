package currexx.core.signal

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.core.CatsSpec
import currexx.domain.user.UserId
import currexx.domain.market.{Condition, CurrencyPair, Indicator, IndicatorParameters, Trend}
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}

class SignalServiceSpec extends CatsSpec {

  "A SignalService" when {
    "getSettings" should {
      "store signal-settings in the repository" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Some(Signals.settings)))

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
          _   <- svc.submit(Signals.macd)
        yield ()

        result.asserting { res =>
          verify(signRepo).save(Signals.macd)
          verify(disp).dispatch(Action.ProcessSignal(Signals.macd))
          res mustBe ()
        }
      }
    }

    "getAll" should {
      "return all signals from the signRepository" in {
        val (signRepo, settRepo, disp) = mocks
        when(signRepo.getAll(any[UserId])).thenReturn(IO.pure(List(Signals.macd)))

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.getAll(Users.uid)
        yield res

        result.asserting { res =>
          verifyNoInteractions(settRepo, disp)
          verify(signRepo).getAll(Users.uid)
          res mustBe List(Signals.macd)
        }
      }
    }

    "processMarketData" should {
      "not do anything when there are no changes in market data since last point" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Some(Signals.settings)))

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(1)))
        yield res

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(disp, signRepo)
          res mustBe ()
        }
      }

      "insert default signal settings when these are missing" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(None))
        when(settRepo.update(any[SignalSettings])).thenReturn(IO.unit)

        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(1)))
        yield res

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verify(settRepo).update(Signals.settings)
          verifyNoInteractions(disp, signRepo)
          res mustBe ()
        }
      }

      "detect MACD line crossing up and check if this is a first signal for that date" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Some(Signals.settings)))
        when(signRepo.isFirstOfItsKindForThatDate(any[Signal])).thenReturn(IO.pure(true))
        when(signRepo.save(any[Signal])).thenReturn(IO.unit)
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(16))
        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          val expectedSignal = Signal(Users.uid, Markets.gbpeur, Indicator.MACD, Condition.CrossingUp, timeSeriesData.prices.head.time)
          verify(settRepo).get(Users.uid)
          verify(signRepo).isFirstOfItsKindForThatDate(expectedSignal)
          verify(signRepo).save(expectedSignal)
          verify(disp).dispatch(Action.ProcessSignal(expectedSignal))
          res mustBe ()
        }
      }

      "not submit a signal if such signal has already been submitted on that date" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Some(Signals.settings)))
        when(signRepo.isFirstOfItsKindForThatDate(any[Signal])).thenReturn(IO.pure(false))

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(16))
        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          val expectedSignal = Signal(Users.uid, Markets.gbpeur, Indicator.MACD, Condition.CrossingUp, timeSeriesData.prices.head.time)
          verify(settRepo).get(Users.uid)
          verify(signRepo).isFirstOfItsKindForThatDate(expectedSignal)
          verifyNoMoreInteractions(signRepo)
          verifyNoInteractions(disp)
          res mustBe ()
        }
      }

      "detect MACD line crossing down and don't check if the signal has been already submitted on that date" in {
        val (signRepo, settRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Some(Signals.settings.copy(triggerFrequency = TriggerFrequency.Continuously))))
        when(signRepo.save(any[Signal])).thenReturn(IO.unit)
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(9))
        val result = for
          svc <- SignalService.make[IO](signRepo, settRepo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.asserting { res =>
          val expectedSignal = Signal(Users.uid, Markets.gbpeur, Indicator.MACD, Condition.CrossingDown, timeSeriesData.prices.head.time)
          verify(settRepo).get(Users.uid)
          verify(signRepo).save(expectedSignal)
          verify(disp).dispatch(Action.ProcessSignal(expectedSignal))
          verifyNoMoreInteractions(signRepo)
          res mustBe ()
        }
      }
    }

    "detectHma" should {
      "create signal when trend direction changes" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val signal = SignalService.detectHma(Users.uid, timeSeriesData, IndicatorParameters.HMA(length = 16))

        val expectedCondition = Condition.TrendDirectionChange(Trend.Consolidation, Trend.Upward)
        signal mustBe Some(Signal(Users.uid, Markets.gbpeur, Indicator.HMA, expectedCondition, timeSeriesData.prices.head.time))
      }

      "not do anything when trend hasn't changed" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
        val signal = SignalService.detectHma(Users.uid, timeSeriesData, IndicatorParameters.HMA(length = 16))

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
