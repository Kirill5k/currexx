package currexx.core.auth.user

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.core.CatsSpec
import currexx.core.common.config.{AuthConfig, JwtConfig}

class PasswordEncryptorSpec extends CatsSpec {

  val authConfig = AuthConfig("$2a$10$8K1p/a0dL1LXMIgoEDFrwO", JwtConfig("ALG", "SECRET"))

  "A PasswordEncryptor" should {

    "hash and validate password with salt" in {
      val result = for
        e       <- PasswordEncryptor.make[IO](authConfig)
        hash    <- e.hash(Password("Password123!"))
        isValid <- e.isValid(Password("Password123!"), hash)
      yield isValid

      result.unsafeToFuture().map(_ mustBe true)
    }
  }
}
