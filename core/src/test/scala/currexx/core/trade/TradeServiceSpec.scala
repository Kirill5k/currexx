package currexx.core.trade

import cats.effect.IO
import currexx.clients.broker.BrokerClient
import currexx.core.CatsSpec
import currexx.core.common.action.ActionDispatcher
import currexx.core.fixtures.{Trades, Users}
import currexx.core.trade.db.TradeSettingsRepository
import currexx.domain.errors.AppError
import currexx.domain.user.UserId

class TradeServiceSpec extends CatsSpec {

  "A TradeService" should {
    "getSettings" should {
      "store trade-settings in the repository" in {
        val (settRepo, client, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Trades.settings))

        val result = for
          svc <- TradeService.make[IO](settRepo, client, disp)
          _   <- svc.getSettings(Users.uid)
        yield ()

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(client, disp)
          res mustBe ()
        }
      }

      "return error when settings do not exist" in {
        val (settRepo, client, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.raiseError(AppError.NotSetup("Trade")))

        val result = for
          svc <- TradeService.make[IO](settRepo, client, disp)
          _   <- svc.getSettings(Users.uid)
        yield ()

        result.attempt.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(client, disp)
          res mustBe Left(AppError.NotSetup("Trade"))
        }
      }
    }

    "updateSettings" should {
      "store trade-settings in the repository" in {
        val (settRepo, client, disp) = mocks
        when(settRepo.update(any[TradeSettings])).thenReturn(IO.unit)

        val result = for
          svc <- TradeService.make[IO](settRepo, client, disp)
          _   <- svc.updateSettings(Trades.settings)
        yield ()

        result.asserting { res =>
          verify(settRepo).update(Trades.settings)
          verifyNoInteractions(client, disp)
          res mustBe ()
        }
      }
    }
  }

  def mocks: (TradeSettingsRepository[IO], BrokerClient[IO], ActionDispatcher[IO]) =
    (mock[TradeSettingsRepository[IO]], mock[BrokerClient[IO]], mock[ActionDispatcher[IO]])
}
