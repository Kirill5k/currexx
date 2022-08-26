package currexx.core.settings

import cats.effect.IO
import currexx.core.CatsSpec
import currexx.core.fixtures.Users
import currexx.core.settings.db.SettingsRepository
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import mongo4cats.bson.ObjectId
import org.mockito.Mockito.times

class SettingsServiceSpec extends CatsSpec {

  "SettingsService" when {
    val settings = GlobalSettings(Users.uid, None, None)
    "get" should {
      "return settings from repository" in {
        val repo = mock[SettingsRepository[IO]]
        when(repo.get(any[UserId])).thenReturn(IO.pure(settings))

        val result = for
          svc <- SettingsService.make(repo)
          res <- svc.get(Users.uid)
        yield res

        result.asserting { res =>
          verify(repo).get(Users.uid)
          res mustBe GlobalSettings(Users.uid, None, None)
        }
      }

      "create new settings in these do not exist" in {
        val repo = mock[SettingsRepository[IO]]
        when(repo.get(any[UserId])).thenReturn(IO.raiseError(AppError.NotSetup("Global"))).thenReturn(IO.pure(settings))
        when(repo.createFor(any[UserId])).thenReturn(IO.unit)

        val result = for
          svc <- SettingsService.make(repo)
          res <- svc.get(Users.uid)
        yield res

        result.asserting { res =>
          verify(repo, times(2)).get(Users.uid)
          verify(repo).createFor(Users.uid)
          res mustBe GlobalSettings(Users.uid, None, None)
        }
      }
    }
  }
}
