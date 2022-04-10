package currexx.core.auth.user

import cats.effect.Sync
import cats.syntax.functor.*
import com.github.t3hnar.bcrypt.*
import currexx.domain.user.*
import currexx.core.common.config.AuthConfig

trait PasswordEncryptor[F[_]]:
  def hash(password: Password): F[PasswordHash]
  def isValid(password: Password, passwordHash: PasswordHash): F[Boolean]

object PasswordEncryptor:
  def make[F[_]](config: AuthConfig)(using F: Sync[F]): F[PasswordEncryptor[F]] =
    F.pure {
      new PasswordEncryptor[F] {
        override def hash(password: Password): F[PasswordHash] =
          F.delay(password.value.bcryptBounded(config.passwordSalt)).map(s => PasswordHash(s))

        override def isValid(password: Password, passwordHash: PasswordHash): F[Boolean] =
          F.fromTry(password.value.isBcryptedSafeBounded(passwordHash.value))
      }
    }
