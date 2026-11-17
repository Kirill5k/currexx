package currexx.core.settings

import cats.effect.IO
import currexx.core.fixtures.{Settings, Users}
import currexx.core.settings.db.SettingsRepository
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import kirill5k.common.cats.test.IOWordSpec

class SettingsServiceSpec extends IOWordSpec {

  "SettingsService" when {
    val settings = GlobalSettings(Users.uid, None, None, None)
    "update" should {
      "validate the replacement document without borrowing the stored signal configuration" in {
        val repo  = mock[SettingsRepository[IO]]
        val trade = Settings.trade.copy(strategy =
          TradeStrategy(
            List(Rule(TradeAction.OpenLong, Rule.Condition.trendIsUpward)),
            Nil
          )
        )
        val replacement = settings.copy(trade = Some(trade))
        (for
          svc    <- SettingsService.make(repo)
          result <- svc.update(replacement).attempt
        yield result).asserting { result =>
          result.left.toOption.get mustBe a[AppError.FailedValidation]
          verifyNoInteractions(repo)
          succeed
        }
      }

      "allow a complete valid replacement and reject duplicate derived identities" in {
        val repo = mock[SettingsRepository[IO]]
        when(repo.update(any[GlobalSettings])).thenReturnUnit
        val valid   = Settings.global
        val invalid = valid.copy(signal = valid.signal.map(s => s.copy(indicators = s.indicators ++ s.indicators)))
        (for
          svc    <- SettingsService.make(repo)
          _      <- svc.update(valid)
          result <- svc.update(invalid).attempt
        yield result).asserting { result =>
          result.isLeft mustBe true
          verify(repo).update(valid)
          verifyNoMoreInteractions(repo)
          succeed
        }
      }
    }

    "get" should {
      "return settings from repository" in {
        val repo = mock[SettingsRepository[IO]]
        when(repo.get(any[UserId])).thenReturnIO(settings)

        val result = for
          svc <- SettingsService.make(repo)
          res <- svc.get(Users.uid)
        yield res

        result.asserting { res =>
          verify(repo).get(Users.uid)
          res mustBe GlobalSettings(Users.uid, None, None, None)
        }
      }

      "create new settings if these do not exist" in {
        val repo = mock[SettingsRepository[IO]]
        when(repo.get(any[UserId])).thenRaiseError(AppError.NotSetup("Global")).thenReturnIO(settings)
        when(repo.createFor(any[UserId])).thenReturn(IO.unit)

        val result = for
          svc <- SettingsService.make(repo)
          res <- svc.get(Users.uid)
        yield res

        result.asserting { res =>
          verify(repo, times(2)).get(Users.uid)
          verify(repo).createFor(Users.uid)
          res mustBe GlobalSettings(Users.uid, None, None, None)
        }
      }
    }
  }
}
