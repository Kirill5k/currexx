package currexx.core.auth.user

import cats.effect.IO
import currexx.core.MockActionDispatcher
import currexx.core.common.action.Action
import currexx.core.fixtures.Users
import currexx.core.auth.user.db.UserRepository
import currexx.domain.user.*
import currexx.domain.errors.AppError.{InvalidEmailOrPassword, InvalidPassword}
import kirill5k.common.cats.test.IOWordSpec

class UserServiceSpec extends IOWordSpec {

  "A UserService" when {
    "create" should {
      "return account id on success" in {
        val (repo, encr, disp) = mocks
        when(encr.hash(any[Password])).thenReturn(IO.pure(Users.hash))
        when(repo.create(any[UserDetails], any[PasswordHash])).thenReturn(IO.pure(Users.uid))

        val result = for
          service <- UserService.make[IO](repo, encr, disp)
          res     <- service.create(Users.details, Users.pwd)
        yield res

        result.asserting { res =>
          disp.submittedActions mustBe List(Action.SetupNewUser(Users.uid))
          verify(encr).hash(Users.pwd)
          verify(repo).create(Users.details, Users.hash)
          res mustBe Users.uid
        }
      }
    }

    "updatePassword" should {
      val cp = ChangePassword(Users.uid, Users.pwd, Password("new-password"))

      "return unit on success" in {
        val (repo, encr, disp) = mocks
        when(encr.isValid(any[Password], any[PasswordHash])).thenReturn(IO.pure(true))
        when(encr.hash(any[Password])).thenReturn(IO.pure(Users.hash))
        when(repo.find(any[UserId])).thenReturn(IO.pure(Users.user))
        when(repo.updatePassword(any[UserId])(any[PasswordHash])).thenReturn(IO.unit)

        val result = for
          service <- UserService.make[IO](repo, encr, disp)
          res     <- service.changePassword(cp)
        yield res

        result.asserting { res =>
          verify(repo).find(cp.id)
          verify(encr).isValid(cp.currentPassword, Users.user.password)
          verify(encr).hash(cp.newPassword)
          verify(repo).updatePassword(cp.id)(Users.hash)
          res mustBe ()
        }
      }

      "return error when passwords do not match" in {
        val (repo, encr, disp) = mocks
        when(repo.find(any[UserId])).thenReturn(IO.pure(Users.user))
        when(encr.isValid(any[Password], any[PasswordHash])).thenReturn(IO.pure(false))

        val result = for
          service <- UserService.make[IO](repo, encr, disp)
          res     <- service.changePassword(cp)
        yield res

        result.attempt.asserting { res =>
          verify(repo).find(cp.id)
          verify(encr).isValid(cp.currentPassword, Users.user.password)
          verifyNoMoreInteractions(repo, encr)
          res mustBe Left(InvalidPassword)
        }
      }
    }

    "find" should {
      "return account on success" in {
        val (repo, encr, disp) = mocks
        when(repo.find(any[UserId])).thenReturn(IO.pure(Users.user))

        val result = for
          service <- UserService.make[IO](repo, encr, disp)
          res     <- service.find(Users.uid)
        yield res

        result.asserting { res =>
          verifyNoInteractions(encr)
          verify(repo).find(Users.uid)
          res mustBe Users.user
        }
      }
    }

    "login" should {

      "return account on success" in {
        val (repo, encr, disp) = mocks
        when(repo.findBy(any[UserEmail])).thenReturn(IO.pure(Some(Users.user)))
        when(encr.isValid(any[Password], any[PasswordHash])).thenReturn(IO.pure(true))

        val result = for
          service <- UserService.make[IO](repo, encr, disp)
          res     <- service.login(Login(Users.details.email, Users.pwd))
        yield res

        result.asserting { res =>
          verify(repo).findBy(Users.details.email)
          verify(encr).isValid(Users.pwd, Users.hash)
          res mustBe Users.user
        }
      }

      "return error when account does not exist" in {
        val (repo, encr, disp) = mocks
        when(repo.findBy(any[UserEmail])).thenReturn(IO.pure(None))

        val result = for
          service <- UserService.make[IO](repo, encr, disp)
          res     <- service.login(Login(Users.details.email, Users.pwd))
        yield res

        result.attempt.asserting { res =>
          verify(repo).findBy(Users.details.email)
          verifyNoInteractions(encr)
          res mustBe Left(InvalidEmailOrPassword)
        }
      }

      "return error when password doesn't match" in {
        val (repo, encr, disp) = mocks
        when(repo.findBy(any[UserEmail])).thenReturn(IO.pure(Some(Users.user)))
        when(encr.isValid(any[Password], any[PasswordHash])).thenReturn(IO.pure(false))

        val result = for
          service <- UserService.make[IO](repo, encr, disp)
          res     <- service.login(Login(Users.details.email, Users.pwd))
        yield res

        result.attempt.asserting { res =>
          verify(repo).findBy(Users.details.email)
          verify(encr).isValid(Users.pwd, Users.hash)
          res mustBe Left(InvalidEmailOrPassword)
        }
      }
    }
  }

  def mocks: (UserRepository[IO], PasswordEncryptor[IO], MockActionDispatcher[IO]) =
    (mock[UserRepository[IO]], mock[PasswordEncryptor[IO]], MockActionDispatcher[IO])
}
