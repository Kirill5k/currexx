package currexx.core.auth

import cats.effect.IO
import jwt.*
import currexx.core.common.config.JwtConfig
import currexx.core.auth.jwt.JwtEncoder
import currexx.domain.session.SessionId
import currexx.domain.user.UserId
import currexx.domain.JsonCodecs
import currexx.domain.errors.AppError
import currexx.domain.IOWordSpec
import org.scalatest.wordspec.AsyncWordSpec
import pdi.jwt.algorithms.JwtUnknownAlgorithm

class JwtEncoderSpec extends IOWordSpec with JsonCodecs {

  val config  = JwtConfig("HS256", "secret-key")
  val session = JwtToken(SessionId("s1"), UserId("u1"))
  val jwtToken = BearerToken(
    "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzZXNzaW9uSWQiOiJzMSIsInVzZXJJZCI6InUxIn0.6mnaHsD11IgZqficW13C9GVOxc9U7ureb8V42EJqlIU"
  )

  "A CirceJwtEncoder" should {

    "create jwt token" in {
      val result = for
        encoder  <- JwtEncoder.circeJwtEncoder[IO](config)
        jwtToken <- encoder.encode(session)
      yield jwtToken

      result.asserting(_ mustBe jwtToken)
    }

    "decode jwt token" in {
      val result = for
        encoder     <- JwtEncoder.circeJwtEncoder[IO](config)
        accessToken <- encoder.decode(jwtToken)
      yield accessToken

      result.asserting(_ mustBe session)
    }

    "return error when invalid jwt token" in {
      val result = for
        encoder     <- JwtEncoder.circeJwtEncoder[IO](config)
        accessToken <- encoder.decode(BearerToken("foo-bar"))
      yield accessToken

      result.throws(AppError.InvalidJwtToken("Expected token [foo-bar] to be composed of 2 or 3 parts separated by dots."))
    }

    "return error when unexpected json payload" in {
      val result = for
        encoder     <- JwtEncoder.circeJwtEncoder[IO](config)
        accessToken <- encoder.decode(BearerToken("IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"))
      yield accessToken

      result.throws(AppError.InvalidJwtToken("""expected whitespace or eof got ',"iat"...' (line 1, column 11)"""))
    }

    "return error when unknown algo" in {
      val result = JwtEncoder.circeJwtEncoder[IO](config.copy(alg = "foo"))

      result.throws(AppError.InvalidJwtEncryptionAlgorithm(JwtUnknownAlgorithm("FOO").fullName))
    }
  }
}
