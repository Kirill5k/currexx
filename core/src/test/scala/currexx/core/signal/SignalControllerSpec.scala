package currexx.core.signal

import cats.effect.IO
import currexx.core.ControllerSpec
import currexx.core.auth.Authenticator
import currexx.core.common.http.SearchParams
import currexx.domain.user.UserId
import currexx.core.fixtures.{Markets, Sessions, Signals, Users}
import currexx.domain.errors.AppError
import currexx.domain.market.CurrencyPair
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

import java.time.Instant

class SignalControllerSpec extends ControllerSpec {

  "A SignalController" when {
    given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "POST /signals" should {
      "return 204 on success" in {
        val svc = mock[SignalService[IO]]
        when(svc.submit(any[Signal])).thenReturn(IO.unit)

        val reqBody = parseJson(s"""{
             |"currencyPair":"GBP/EUR",
             |"triggeredBy": {"kind":"trend-change-detection", "source": "close", "transformation": {"kind": "hma", "length": 16}},
             |"condition": {"kind":"crossing-up"}
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
             |  "triggeredBy": {"kind": "foo"}
             |}""".stripMargin)
        val req = requestWithAuthHeader(uri"/signals", Method.POST).withEntity(reqBody)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          """{"message":"Missing required field, Received unknown type: 'foo'. Exists only types: trend-change-detection."}"""
        verifyJsonResponse(res, Status.UnprocessableEntity, Some(responseBody))
        verifyNoInteractions(svc)
      }

      "return error on unrecognized condition" in {
        val svc = mock[SignalService[IO]]

        val reqBody = parseJson(s"""{
             |"currencyPair":"GBP/EUR",
             |"triggeredBy": {"kind":"trend-change-detection", "source": "close", "transformation": {"kind": "hma", "length": 16}},
             |"condition": {"kind":"foo","value":0.05}
             |}""".stripMargin)
        val req = requestWithAuthHeader(uri"/signals", Method.POST).withEntity(reqBody)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          """{"message":"Received unknown type: 'foo'. Exists only types: above-threshold, crossing-up, trend-direction-change, crossing-down, below-threshold."}"""
        verifyJsonResponse(res, Status.UnprocessableEntity, Some(responseBody))
        verifyNoInteractions(svc)
      }

      "return error on invalid currency pair signal" in {
        val svc = mock[SignalService[IO]]

        val reqBody = parseJson(s"""{
             |"currencyPair":"FOO/BAR",
             |"triggeredBy": {"kind":"trend-change-detection", "source": "close", "transformation": {"kind": "hma", "length": 16}},
             |"condition": {"kind":"crossing-up"}
             |}""".stripMargin)
        val req = requestWithAuthHeader(uri"/signals", Method.POST).withEntity(reqBody)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = """{"message":"Unknown currency code FOO; Available currencies are: PLN, CAD, AUD, GBP, DKK, JPY, USD, RUB, NZD, NOK, EUR"}"""
        verifyJsonResponse(res, Status.UnprocessableEntity, Some(responseBody))
        verifyNoInteractions(svc)
      }

      "return error on malformed currency pair signal" in {
        val svc = mock[SignalService[IO]]

        val reqBody = parseJson(s"""{
               |"currencyPair":"FOO-BAR",
               |"triggeredBy": {"kind":"trend-change-detection", "source": "close", "transformation": {"kind": "hma", "length": 16}},
               |"condition": {"kind":"crossing-up"}
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
        when(svc.getAll(any[UserId], any[SearchParams])).thenReturn(IO.pure(List(Signals.trendDirectionChanged)))

        val req = requestWithAuthHeader(uri"/signals?from=2020-01-01&to=2021-01-01T04:01:00Z&currencyPair=GBP/USD", Method.GET)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = s"""[{
               |"currencyPair":"GBPEUR",
               |"time": "${Signals.ts}",
               |"triggeredBy": {"kind":"trend-change-detection", "source": "close", "transformation": {"kind": "hma", "length": 16}},
               |"condition": {"kind":"trend-direction-change","from":"downward","to":"upward","previousTrendLength":1}
               |}]""".stripMargin
        verifyJsonResponse(res, Status.Ok, Some(responseBody))
        verify(svc).getAll(
          Users.uid,
          SearchParams(Some(Instant.parse("2020-01-01T00:00:00Z")), Some(Instant.parse("2021-01-01T04:01:00Z")), Some(Markets.gbpusd))
        )
      }

      "return error when date 'from' is after 'to'" in {
        val svc = mock[SignalService[IO]]

        val req = requestWithAuthHeader(uri"/signals?from=2021-01-02&to=2021-01-01", Method.GET)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = s"""{"message":"Date 'from' must be before date 'to'"}"""
        verifyJsonResponse(res, Status.UnprocessableEntity, Some(responseBody))
        verifyNoInteractions(svc)
      }
    }

    "GET /signals/settings" should {
      "return signal settings for configured indicators" in {
        val svc = mock[SignalService[IO]]
        when(svc.getSettings(any[UserId])).thenReturn(IO.pure(Signals.settings))

        val req = requestWithAuthHeader(uri"/signals/settings", Method.GET)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""{
              |"triggerFrequency" : "once-per-day",
              |"indicators": [
              |{
              | "kind":"trend-change-detection",
              | "source": "close",
              | "transformation": {"kind": "hma", "length": 16}
              |}
              |]}""".stripMargin
        verifyJsonResponse(res, Status.Ok, Some(responseBody))
        verify(svc).getSettings(Users.uid)
      }

      "return error when signals are not configured in the given accounts" in {
        val svc = mock[SignalService[IO]]
        when(svc.getSettings(any[UserId])).thenReturn(IO.raiseError(AppError.NotSetup("Signal")))

        val req = requestWithAuthHeader(uri"/signals/settings", Method.GET)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NotFound, Some("""{"message":"Current account doesn't have Signal-settings set up"}"""))
        verify(svc).getSettings(Users.uid)
      }
    }

    "PUT /signals/settings" should {
      "update signal settings" in {
        val svc = mock[SignalService[IO]]
        when(svc.updateSettings(any[SignalSettings])).thenReturn(IO.unit)

        val requestBody =
          s"""{
             |"triggerFrequency" : "continuously",
             |"indicators": [
             |{
             | "kind":"trend-change-detection",
             | "source": "close",
             | "transformation": {"kind": "hma", "length": 16}
             |}
             |]}""".stripMargin

        val req = requestWithAuthHeader(uri"/signals/settings", Method.PUT).withEntity(parseJson(requestBody))
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).updateSettings(Signals.settings.copy(triggerFrequency = TriggerFrequency.Continuously))
      }
    }
  }
}
