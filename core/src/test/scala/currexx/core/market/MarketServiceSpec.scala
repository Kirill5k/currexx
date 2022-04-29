package currexx.core.market

import cats.effect.IO
import currexx.core.CatsSpec
import currexx.domain.user.UserId
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.market.db.MarketStateRepository
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, PriceRange, Indicator, Condition}

import java.time.Instant
import scala.concurrent.duration.*

class MarketServiceSpec extends CatsSpec {

  "A MarketService" when {
    "getState" should {
      "return state of all traded currencies" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.getAll(any[UserId])).thenReturn(IO.pure(List(Markets.state)))

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.getState(Users.uid)
        yield state

        result.asserting { res =>
          verify(stateRepo).getAll(Users.uid)
          verifyNoInteractions(disp)
          res mustBe List(Markets.state)
        }
      }
    }

    "processMarketData" should {
      "update state with latest received prices" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[PriceRange])).thenReturn(IO.unit)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processMarketData(Users.uid, Markets.timeSeriesData)
        yield state

        result.asserting { res =>
          verify(stateRepo).update(Users.uid, Markets.gbpeur, Markets.priceRange)
          verifyNoInteractions(disp)
          res mustBe ()
        }
      }
    }

    "processSignal" should {
      "update signal state with the latest received signal when current signal state is empty" in {
        val (stateRepo, disp) = mocks

        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturn(IO.pure(Some(Markets.state.copy(signals = Map.empty))))
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[Map[Indicator, List[IndicatorState]]])).thenReturn(IO.pure(Markets.state))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processSignal(Signals.macd)
        yield state

        result.asserting { res =>
          val finalSignalState = Map(Indicator.MACD -> List(IndicatorState(Condition.CrossingUp, Signals.ts)))
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).update(Users.uid, Markets.gbpeur, finalSignalState)
          verify(disp).dispatch(Action.ProcessMarketState(Markets.state))
          res mustBe ()
        }
      }

      "update signal state with the latest received signal" in {
        val (stateRepo, disp) = mocks
        val currentMacdState = IndicatorState(Condition.CrossingUp, Signals.ts.minusSeconds(3.days.toSeconds))
        val signalState = Map(Indicator.MACD -> List(currentMacdState))
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturn(IO.pure(Some(Markets.state.copy(signals = signalState))))
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[Map[Indicator, List[IndicatorState]]])).thenReturn(IO.pure(Markets.state))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processSignal(Signals.macd)
        yield state

        result.asserting { res =>
          val finalSignalState = Map(Indicator.MACD -> List(
            IndicatorState(Condition.CrossingUp, Signals.ts),
            currentMacdState
          ))
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).update(Users.uid, Markets.gbpeur, finalSignalState)
          verify(disp).dispatch(Action.ProcessMarketState(Markets.state))
          res mustBe ()
        }
      }

      "replace latest indicator state if new signal has same date" in {
        val (stateRepo, disp) = mocks
        val signalState = Map(Indicator.MACD -> List(IndicatorState(Condition.CrossingUp, Signals.ts.minusSeconds(10))))
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturn(IO.pure(Some(Markets.state.copy(signals = signalState))))
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[Map[Indicator, List[IndicatorState]]])).thenReturn(IO.pure(Markets.state))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processSignal(Signals.macd)
        yield state

        result.asserting { res =>
          val finalSignalState = Map(Indicator.MACD -> List(IndicatorState(Condition.CrossingUp, Signals.ts)))
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).update(Users.uid, Markets.gbpeur, finalSignalState)
          verify(disp).dispatch(Action.ProcessMarketState(Markets.state))
          res mustBe ()
        }
      }
    }
  }

  def mocks: (MarketStateRepository[IO], ActionDispatcher[IO]) =
    (mock[MarketStateRepository[IO]], mock[ActionDispatcher[IO]])
}
