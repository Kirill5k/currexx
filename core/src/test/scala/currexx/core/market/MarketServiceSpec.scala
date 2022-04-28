package currexx.core.market

import cats.effect.IO
import currexx.core.CatsSpec
import currexx.domain.user.UserId
import currexx.core.common.action.ActionDispatcher
import currexx.core.market.db.{MarketSettingsRepository, MarketStateRepository}
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.domain.errors.AppError

class MarketServiceSpec extends CatsSpec {

  "A MarketService" when {
    "getSettings" should {
      "store market-settings in the repository" in {
        val (settRepo, stateRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.pure(Markets.settings))

        val result = for
          svc <- MarketService.make[IO](settRepo, stateRepo, disp)
          _   <- svc.getSettings(Users.uid)
        yield ()

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(stateRepo, disp)
          res mustBe ()
        }
      }

      "return error when settings do not exist" in {
        val (settRepo, stateRepo, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturn(IO.raiseError(AppError.NotSetup("Market")))

        val result = for
          svc <- MarketService.make[IO](settRepo, stateRepo, disp)
          _   <- svc.getSettings(Users.uid)
        yield ()

        result.attempt.asserting { res =>
          verify(settRepo).get(Users.uid)
          verifyNoInteractions(stateRepo, disp)
          res mustBe Left(AppError.NotSetup("Market"))
        }
      }
    }

    "updateSettings" should {
      "store market-settings in the repository" in {
        val (settRepo, stateRepo, disp) = mocks
        when(settRepo.update(any[MarketSettings])).thenReturn(IO.unit)

        val result = for
          svc <- MarketService.make[IO](settRepo, stateRepo, disp)
          _   <- svc.updateSettings(Markets.settings)
        yield ()

        result.asserting { res =>
          verify(settRepo).update(Markets.settings)
          verifyNoInteractions(stateRepo, disp)
          res mustBe ()
        }
      }
    }
  }

  def mocks: (MarketSettingsRepository[IO], MarketStateRepository[IO], ActionDispatcher[IO]) =
    (mock[MarketSettingsRepository[IO]], mock[MarketStateRepository[IO]], mock[ActionDispatcher[IO]])
}
