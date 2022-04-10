package currexx.core.signal

import cats.effect.IO
import currexx.core.ControllerSpec
import currexx.core.auth.Authenticator
import currexx.domain.user.UserId
import currexx.core.fixtures.{Sessions, Signals, Users}
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

class SignalControllerSpec extends ControllerSpec {

  "A SignalController" when {
    given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "POST /signals" should {
      "return 204 on success" in {
        val svc = mock[SignalService[IO]]
        when(svc.submit(any[Signal])).thenReturn(IO.unit)

        val reqBody = parseJson(s"""{
             |"currencyPair":"GBP/EUR",
             |"indicator": "macd",
             |"condition": {"kind":"crossing-up","value":0.05}
             |}""".stripMargin)
        val req = requestWithAuthHeader(uri"/signals", Method.POST).withEntity(reqBody)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).submit(any[Signal])
      }

      "return error on unrecognized indicator" in {
        val svc = mock[SignalService[IO]]

        val reqBody = parseJson(s"""{
             |  "currencyPair":"GBP/EUR",
             |  "indicator":"foo"
             |}""".stripMargin)
        val req = requestWithAuthHeader(uri"/signals", Method.POST).withEntity(reqBody)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.UnprocessableEntity, Some("""{"message":"Unrecognized indicator foo, condition is required"}"""))
        verifyNoInteractions(svc)
      }

      "return error on unrecognized condition" in {
        val svc = mock[SignalService[IO]]

        val reqBody = parseJson(s"""{
             |"currencyPair":"GBP/EUR",
             |"indicator": "macd",
             |"condition": {"kind":"foo","value":0.05}
             |}""".stripMargin)
        val req = requestWithAuthHeader(uri"/signals", Method.POST).withEntity(reqBody)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.UnprocessableEntity, Some("""{"message":"Unexpected condition kind foo"}"""))
        verifyNoInteractions(svc)
      }

      "return error on invalid currency pair signal" in {
        val svc = mock[SignalService[IO]]

        val reqBody = parseJson(s"""{
             |"currencyPair":"FOO/BAR",
             |"indicator": "macd",
             |"condition": {"kind":"crossing-up","value":0.05}
             |}""".stripMargin)
        val req = requestWithAuthHeader(uri"/signals", Method.POST).withEntity(reqBody)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(
          res,
          Status.UnprocessableEntity,
          Some(
            """{"message":"Code FOO cannot be matched against any context defined Currency. Available Currencies are CAD, CZK, GBP, MXN, CHF, CNY, RUB, NZD, HKD, AUD, SEK, TRY, BRL, KRW, ETH, CLP, INR, LTC, BTC, DKK, XAU, XAG, JPY, ARS, MYR, USD, NOK, NAD, EUR, ZAR"}"""
          )
        )
        verifyNoInteractions(svc)
      }

      "return error on malformed currency pair signal" in {
        val svc = mock[SignalService[IO]]

        val reqBody = parseJson(s"""{
               |"currencyPair":"FOO-BAR",
               |"indicator": "macd",
               |"condition": {"kind":"crossing-up","value":0.05}
               |}""".stripMargin)
        val req = requestWithAuthHeader(uri"/signals", Method.POST).withEntity(reqBody)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.UnprocessableEntity, Some("""{"message":"FOO-BAR is not valid currency pair representation"}"""))
        verifyNoInteractions(svc)
      }
    }

    "GET /signals" should {
      "return all submitted signals" in {
        val svc = mock[SignalService[IO]]
        when(svc.getAll(any[UserId])).thenReturn(IO.pure(List(Signals.macd)))

        val req = requestWithAuthHeader(uri"/signals", Method.GET)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = s"""[{
               |"currencyPair":"GBP/EUR",
               |"time": "${Signals.ts}",
               |"indicator": "macd",
               |"condition": {"kind":"crossing-up","value":0.05}
               |}]""".stripMargin
        verifyJsonResponse(res, Status.Ok, Some(responseBody))
        verify(svc).getAll(Users.uid)
      }
    }
  }
}
