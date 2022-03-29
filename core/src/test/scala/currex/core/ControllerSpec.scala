package currex.core

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import io.circe.*
import io.circe.parser.*
import org.http4s.{Response, Status}
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

trait ControllerSpec extends AnyWordSpec with Matchers {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  def verifyJsonResponse(
      response: IO[Response[IO]],
      expectedStatus: Status,
      expectedBody: Option[String] = None
  ): Assertion =
    response.flatMap { res =>
      expectedBody match {
        case Some(expectedJson) =>
          res.as[String].map { receivedJson =>
            res.status mustBe expectedStatus
            parse(receivedJson) mustBe parse(expectedJson)
          }
        case None =>
          res.body.compile.toVector.map { receivedJson =>
            res.status mustBe expectedStatus
            receivedJson mustBe empty
          }
      }
    }.unsafeRunSync()
}
