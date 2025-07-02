package currexx.core.market

import cats.effect.IO
import currexx.core.MockActionDispatcher
import currexx.domain.user.UserId
import currexx.core.common.action.Action
import currexx.core.market.db.MarketStateRepository
import currexx.core.fixtures.{Markets, Trades, Users}
import currexx.domain.market.{CurrencyPair, TradeOrder}
import kirill5k.common.cats.test.IOWordSpec

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
      //TODO: add tests to test new signal processing logic
    }
  }

  def mocks: (MarketStateRepository[IO], MockActionDispatcher[IO]) =
    (mock[MarketStateRepository[IO]], MockActionDispatcher[IO])
}
