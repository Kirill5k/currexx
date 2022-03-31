package currex.core.auth

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currex.core.CatsSpec
import currex.core.auth.jwt.*
import currex.core.auth.session.*
import currex.core.auth.user.*
import currex.core.common.JsonCodecs
import currex.core.common.config.JwtConfig
import currex.core.common.errors.AppError
import org.scalatest.wordspec.AsyncWordSpec
import pdi.jwt.algorithms.JwtUnknownAlgorithm

class JwtEncoderSpec extends CatsSpec with JsonCodecs {

  val config  = JwtConfig("HS256", "secret-key")
  val session = JwtToken(SessionId("s1"), UserId("u1"))
  val jwtToken = BearerToken("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzZXNzaW9uSWQiOiJzMSIsInVzZXJJZCI6InUxIn0.6mnaHsD11IgZqficW13C9GVOxc9U7ureb8V42EJqlIU")

  "A CirceJwtEncoder" should {

    "create jwt token" in {
      val result = for
        encoder  <- JwtEncoder.circeJwtEncoder[IO](config)
        jwtToken <- encoder.encode(session)
      yield jwtToken

      result.unsafeToFuture().map(_ mustBe jwtToken)
    }

    "decode jwt token" in {
      val result = for
        encoder     <- JwtEncoder.circeJwtEncoder[IO](config)
        accessToken <- encoder.decode(jwtToken)
      yield accessToken

      result.unsafeToFuture().map(_ mustBe session)
    }

    "return error when invalid jwt token" in {
      val result = for
        encoder     <- JwtEncoder.circeJwtEncoder[IO](config)
        accessToken <- encoder.decode(BearerToken("foo-bar"))
      yield accessToken

      result.attempt.unsafeToFuture().map { res =>
        res mustBe Left(AppError.InvalidJwtToken("Expected token [foo-bar] to be composed of 2 or 3 parts separated by dots."))
      }
    }

    "return error when unexpected json payload" in {
      val result = for
        encoder     <- JwtEncoder.circeJwtEncoder[IO](config)
        accessToken <- encoder.decode(BearerToken("IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"))
      yield accessToken

      result.attempt.unsafeToFuture().map { res =>
        res mustBe Left(AppError.InvalidJwtToken("""expected whitespace or eof got ',"iat"...' (line 1, column 11)"""))
      }
    }

    "return error when unknown algo" in {
      val result = for
        _ <- JwtEncoder.circeJwtEncoder[IO](config.copy(alg = "foo"))
      yield ()

      result.attempt.unsafeToFuture().map { res =>
        res mustBe Left(AppError.InvalidJwtEncryptionAlgorithm(JwtUnknownAlgorithm("FOO")))
      }
    }
  }
}
