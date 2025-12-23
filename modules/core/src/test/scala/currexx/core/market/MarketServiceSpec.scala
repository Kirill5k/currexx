package currexx.core.market

import cats.effect.IO
import currexx.core.MockActionDispatcher
import currexx.domain.user.UserId
import currexx.core.common.action.Action
import currexx.core.market.db.MarketStateRepository
import currexx.core.fixtures.{Markets, Signals, Trades, Users}
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.Direction
import kirill5k.common.cats.test.IOWordSpec
import kirill5k.common.syntax.time.*

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
            Some(PositionState(TradeOrder.Position.Buy, Trades.ts))
          )
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }

    "processSignal" should {

      "update market profile on new signal" in {
        val signal         = Signals.trend(Direction.Downward)
        val updatedProfile = MarketProfile(trend = Some(TrendState(Direction.Downward, signal.time)))
        val updatedState   = Markets.state.copy(profile = updatedProfile)

        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state)
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[MarketProfile])).thenReturnIO(updatedState)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processSignals(Users.uid, Markets.gbpeur, List(signal))
        yield state

        result.asserting { res =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).update(Users.uid, Markets.gbpeur, updatedProfile)
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(updatedState, Markets.profile))
          res mustBe ()
        }
      }

      "not do anything when market hasn't changed" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state)

        val signal = Signals.trend(Direction.Upward)
        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processSignals(Users.uid, Markets.gbpeur, List(signal))
        yield state

        result.asserting { res =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verifyNoMoreInteractions(stateRepo)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }

    "updateTimeState" should {
      "fastforward confirmedAt when the gap between latest and 2nd to latest candle is greater than interval" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnIO(Some(Markets.state))
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[MarketProfile], any[Option[PositionState]])).thenReturnIO(Markets.state)

        val dataWithGap = Markets.timeSeriesData.copy(
          prices = Markets.priceRanges.head.copy(time = Markets.ts.plus(1.day)) :: Markets.priceRanges
        )

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.updateTimeState(Users.uid, dataWithGap)
        yield state

        result.asserting { res =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).update(
            Users.uid,
            Markets.gbpeur,
            MarketProfile(trend = Some(TrendState(Direction.Upward, Markets.ts.plus(1.day))), lastBandCrossing = None, lastPriceLineCrossing = None, crossover = None),
            Some(PositionState(TradeOrder.Position.Buy, Markets.ts.plus(1.day)))
          )
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "should not do anything when there is no significant gap between candles" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnIO(Some(Markets.state))

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.updateTimeState(Users.uid, Markets.timeSeriesData)
        yield state

        result.asserting { res =>
          verifyNoInteractions(stateRepo)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }
  }

  def mocks: (MarketStateRepository[IO], MockActionDispatcher[IO]) =
    (mock[MarketStateRepository[IO]], MockActionDispatcher[IO])
}
