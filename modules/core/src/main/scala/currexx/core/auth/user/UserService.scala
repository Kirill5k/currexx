package currexx.core.auth.user

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
import currexx.core.auth.user.db.UserRepository
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.effects.*
import currexx.domain.errors.AppError.*
import currexx.domain.user.*

enum LoginResult:
  case Fail
  case Success(user: User)

trait UserService[F[_]]:
  def create(details: UserDetails, password: Password): F[UserId]
  def login(login: Login): F[User]
  def find(uid: UserId): F[User]
  def changePassword(cp: ChangePassword): F[Unit]

final private class LiveUserService[F[_]](
    private val repository: UserRepository[F],
    private val encryptor: PasswordEncryptor[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: MonadError[F, Throwable]
) extends UserService[F] {

  override def create(details: UserDetails, password: Password): F[UserId] =
    encryptor
      .hash(password)
      .flatMap(h => repository.create(details, h))
      .flatTap(uid => dispatcher.dispatch(Action.SetupNewUser(uid)))

  override def login(login: Login): F[User] =
    repository
      .findBy(login.email)
      .flatMapOption(F.pure(LoginResult.Fail)) { acc =>
        encryptor.isValid(login.password, acc.password).map(if (_) LoginResult.Success(acc) else LoginResult.Fail)
      }
      .flatMap {
        case LoginResult.Fail       => InvalidEmailOrPassword.raiseError[F, User]
        case LoginResult.Success(a) => F.pure(a)
      }

  override def find(uid: UserId): F[User] =
    repository.find(uid)

  override def changePassword(cp: ChangePassword): F[Unit] =
    repository
      .find(cp.id)
      .flatMap(acc => encryptor.isValid(cp.currentPassword, acc.password))
      .flatMap {
        case false => InvalidPassword.raiseError[F, PasswordHash]
        case true  => encryptor.hash(cp.newPassword)
      }
      .flatMap(repository.updatePassword(cp.id))

}

object UserService:
  def make[F[_]](
      repo: UserRepository[F],
      encr: PasswordEncryptor[F],
      dispatcher: ActionDispatcher[F]
  )(using
      F: MonadError[F, Throwable]
  ): F[UserService[F]] =
    F.pure(LiveUserService[F](repo, encr, dispatcher))
