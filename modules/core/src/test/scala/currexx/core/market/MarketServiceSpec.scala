package currexx.core.market

import cats.effect.IO
import currexx.core.MockActionDispatcher
import currexx.domain.user.UserId
import currexx.core.common.action.Action
import currexx.core.market.db.MarketStateRepository
import currexx.core.fixtures.{Markets, Signals, Trades, Users}
import currexx.domain.market.{CurrencyPair, IndicatorKind, TradeOrder}
import kirill5k.common.cats.test.IOWordSpec

import java.time.Instant
import scala.concurrent.duration.*

class MarketServiceSpec extends IOWordSpec {

  "A MarketService" when {
    "clearState" should {
      "delete all existing market states and close orders" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.deleteAll(any[UserId])).thenReturnUnit

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
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
          svc   <- MarketService.make[IO](stateRepo, disp)
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
          svc   <- MarketService.make[IO](stateRepo, disp)
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
          svc   <- MarketService.make[IO](stateRepo, disp)
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
      "update state with current position" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[Option[PositionState]])).thenReturn(IO.unit)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processTradeOrderPlacement(Trades.order)
        yield state

        result.asserting { res =>
          verify(stateRepo).update(
            Users.uid,
            Markets.gbpeur,
            Some(PositionState(TradeOrder.Position.Buy, Trades.ts, Markets.priceRange.close))
          )
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }

    "processSignal" should {
      "update signal state with the latest received signal when current signal state is empty" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state.copy(signals = Map.empty))
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[Map[IndicatorKind, List[IndicatorState]]])).thenReturnIO(Markets.state)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processSignals(Users.uid, Markets.gbpeur, List(Signals.trendDirectionChanged))
        yield state

        result.asserting { res =>
          val indState = IndicatorState(Signals.trendDirectionChanged.condition, Signals.ts, Markets.trendChangeDetection)
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).update(Users.uid, Markets.gbpeur, Map(Markets.trendChangeDetection.kind -> List(indState)))
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(Markets.state, List(IndicatorKind.TrendChangeDetection)))
          res mustBe ()
        }
      }

      "update signal state with the latest received signal" in {
        val (stateRepo, disp) = mocks
        val currentIndState = IndicatorState(
          Signals.trendDirectionChanged.condition,
          Signals.ts.minusSeconds(3.days.toSeconds),
          Markets.trendChangeDetection
        )
        val signalState = Map(Markets.trendChangeDetection.kind -> List(currentIndState))
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state.copy(signals = signalState))
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[Map[IndicatorKind, List[IndicatorState]]])).thenReturnIO(Markets.state)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processSignals(Users.uid, Markets.gbpeur, List(Signals.trendDirectionChanged))
        yield state

        result.asserting { res =>
          val finalSignalState = Map(
            Markets.trendChangeDetection.kind -> List(
              IndicatorState(Signals.trendDirectionChanged.condition, Signals.ts, Markets.trendChangeDetection),
              currentIndState
            )
          )
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).update(Users.uid, Markets.gbpeur, finalSignalState)
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(Markets.state, List(IndicatorKind.TrendChangeDetection)))
          res mustBe ()
        }
      }

      "update signal state with multiple signals" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state)
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[Map[IndicatorKind, List[IndicatorState]]])).thenReturnIO(Markets.state)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processSignals(Users.uid, Markets.gbpeur, List(Signals.thresholdCrossing, Signals.trendDirectionChanged))
        yield state

        result.asserting { res =>
          val finalSignalState = Map(
            Markets.trendChangeDetection.kind ->
              List(IndicatorState(Signals.trendDirectionChanged.condition, Signals.ts, Markets.trendChangeDetection)),
            Markets.thresholdCrossing.kind ->
              List(IndicatorState(Signals.thresholdCrossing.condition, Signals.ts, Markets.thresholdCrossing))
          )
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).update(Users.uid, Markets.gbpeur, finalSignalState)
          disp.submittedActions mustBe List(
            Action.ProcessMarketStateUpdate(Markets.state, List(IndicatorKind.ThresholdCrossing, IndicatorKind.TrendChangeDetection))
          )
          res mustBe ()
        }
      }

      "replace latest indicator state if new signal has same date" in {
        val (stateRepo, disp) = mocks
        val currentIndState = IndicatorState(
          Signals.trendDirectionChanged.condition,
          Signals.ts.minusSeconds(10),
          Markets.trendChangeDetection
        )
        val signalState = Map(Markets.trendChangeDetection.kind -> List(currentIndState))

        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state.copy(signals = signalState))
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[Map[IndicatorKind, List[IndicatorState]]])).thenReturnIO(Markets.state)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processSignals(Users.uid, Markets.gbpeur, List(Signals.trendDirectionChanged))
        yield state

        result.asserting { res =>
          val finalSignalState = Map(Markets.trendChangeDetection.kind -> List(currentIndState.copy(time = Signals.ts)))
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).update(Users.uid, Markets.gbpeur, finalSignalState)
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(Markets.state, List(IndicatorKind.TrendChangeDetection)))
          res mustBe ()
        }
      }

      "not emit update when indicator state hasn't changed" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.stateWithSignal)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processSignals(Users.uid, Markets.gbpeur, List(Signals.trendDirectionChanged))
        yield state

        result.asserting { res =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verifyNoMoreInteractions(stateRepo)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }
  }

  def mocks: (MarketStateRepository[IO], MockActionDispatcher[IO]) =
    (mock[MarketStateRepository[IO]], MockActionDispatcher[IO])
}
