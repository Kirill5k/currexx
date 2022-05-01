package currexx.core.trade

import cats.effect.IO
import currexx.clients.broker.BrokerClient
import currexx.core.CatsSpec
import currexx.core.common.action.ActionDispatcher
import currexx.core.fixtures.{Markets, Trades, Users}
import currexx.core.trade.db.{TradeOrderRepository, TradeSettingsRepository}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import currexx.domain.market.Indicator

class TradeServiceSpec extends CatsSpec {

  "A TradeService" should {
    "getAllOrders" should {
      "return all orders from the repository" in {
        val (settRepo, orderRepo, client, disp) = mocks
        when(orderRepo.getAll(any[UserId])).thenReturn(IO.pure(List(Trades.order)))

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, client, disp)
          orders   <- svc.getAllOrders(Users.uid)
        yield orders

        result.asserting { res =>
          verify(orderRepo).getAll(Users.uid)
          verifyNoInteractions(settRepo, client, disp)
          res mustBe List(Trades.order)
        }
      }
    }

    "getSettings" should {
      "store trade-settings in the repository" in {
        val (settRepo, orderRepo, client, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Trades.settings))

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, client, disp)
          _   <- svc.getSettings(Users.uid)
        yield ()

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(orderRepo, client, disp)
          res mustBe ()
        }
      }

      "return error when settings do not exist" in {
        val (settRepo, orderRepo, client, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.raiseError(AppError.NotSetup("Trade")))

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, client, disp)
          _   <- svc.getSettings(Users.uid)
        yield ()

        result.attempt.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(orderRepo, client, disp)
          res mustBe Left(AppError.NotSetup("Trade"))
        }
      }
    }

    "updateSettings" should {
      "store trade-settings in the repository" in {
        val (settRepo, orderRepo, client, disp) = mocks
        when(settRepo.update(any[TradeSettings])).thenReturn(IO.unit)

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, client, disp)
          _   <- svc.updateSettings(Trades.settings)
        yield ()

        result.asserting { res =>
          verify(settRepo).update(Trades.settings)
          verifyNoInteractions(orderRepo, client, disp)
          res mustBe ()
        }
      }
    }

    "processMarketState" should {
      "not do anything when trading strategy is disabled" in {
        val (settRepo, orderRepo, client, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Trades.settings.copy(strategy = TradeStrategy.Disabled)))

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, client, disp)
          _   <- svc.processMarketState(Markets.state, Indicator.RSI)
        yield ()

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(orderRepo, client, disp)
          res mustBe ()
        }
      }
    }
  }

  def mocks: (TradeSettingsRepository[IO], TradeOrderRepository[IO], BrokerClient[IO], ActionDispatcher[IO]) =
    (mock[TradeSettingsRepository[IO]], mock[TradeOrderRepository[IO]], mock[BrokerClient[IO]], mock[ActionDispatcher[IO]])
}
