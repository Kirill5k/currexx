package currexx.core.signal

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import currexx.core.auth.Authenticator
import currexx.core.common.http.SearchParams
import currexx.core.fixtures.{Markets, Sessions, Signals, Users}
import currexx.domain.market.{Interval}
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation, Condition, Direction}
import currexx.domain.user.UserId
import kirill5k.common.cats.Clock
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

import java.time.Instant

class SignalControllerSpec extends HttpRoutesWordSpec {

  "A SignalController" when {
    val now                       = Instant.now
    given Clock[IO]               = Clock.mock[IO](now)
    given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "POST /signals" should {
      "return 204 on success" in {
        val svc = mock[SignalService[IO]]
        when(svc.submit(any[Signal])).thenReturn(IO.unit)

        val req = Request[IO](Method.POST, uri"/signals")
          .withAuthHeader()
          .withBody("""{
              |"interval":"H1",
              |"currencyPair":"GBP/EUR",
              |"triggeredBy": {"kind":"trend-change-detection", "source": "close", "transformation": {"kind": "hma", "length": 16}},
              |"condition": {"kind":"trend-direction-change", "from":"downward", "to":"upward"}
              |}""".stripMargin)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).submit(Signal(
          userId = Users.uid,
          currencyPair = Markets.gbpeur,
          interval = Interval.H1,
          condition = Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, None),
          triggeredBy = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(16)),
          time = now
        ))
      }

      "return error on unrecognized indicator" in {
        val svc = mock[SignalService[IO]]

        val req = Request[IO](Method.POST, uri"/signals")
          .withAuthHeader()
          .withBody("""{"interval":"H1","currencyPair":"GBP/EUR","triggeredBy": {"kind": "foo"}}""")
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val validIndicators = "trend-change-detection, keltner-channel, lines-crossing, threshold-crossing, composite"
        val responseBody = s"""{"message":"Missing required field, Received unknown type: 'foo'. Exists only types: $validIndicators."}"""
        res mustHaveStatus (Status.UnprocessableEntity, Some(responseBody))
        verifyNoInteractions(svc)
      }

      "return error on unrecognized condition" in {
        val svc = mock[SignalService[IO]]

        val req = Request[IO](Method.POST, uri"/signals")
          .withAuthHeader()
          .withBody("""{
              |"interval":"H1",
              |"currencyPair":"GBP/EUR",
              |"triggeredBy": {"kind":"trend-change-detection", "source": "close", "transformation": {"kind": "hma", "length": 16}},
              |"condition": {"kind":"foo","value":0.05}
              |}""".stripMargin)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val validCondition = "above-threshold, lower-band-crossing, composite, trend-direction-change, upper-band-crossing, below-threshold, lines-crossing"
        val responseBody = s"""{"message":"Received unknown type: 'foo'. Exists only types: $validCondition."}"""
        res mustHaveStatus (Status.UnprocessableEntity, Some(responseBody))
        verifyNoInteractions(svc)
      }

      "return error on invalid currency pair signal" in {
        val svc = mock[SignalService[IO]]

        val req = Request[IO](Method.POST, uri"/signals")
          .withAuthHeader()
          .withBody("""{
              |"interval":"H1",
              |"currencyPair":"FOO/BAR",
              |"triggeredBy": {"kind":"trend-change-detection", "source": "close", "transformation": {"kind": "hma", "length": 16}},
              |"condition": {"kind":"trend-direction-change", "from":"downward", "to":"upward"}
              |}""".stripMargin)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          """{"message":"Unknown currency code FOO; Available currencies are: PLN, CAD, AUD, GBP, CHF, DKK, JPY, USD, RUB, NZD, NOK, EUR"}"""
        res mustHaveStatus (Status.UnprocessableEntity, Some(responseBody))
        verifyNoInteractions(svc)
      }

      "return error on malformed currency pair signal" in {
        val svc = mock[SignalService[IO]]

        val req = Request[IO](Method.POST, uri"/signals")
          .withAuthHeader()
          .withBody("""{
              |"interval":"H1",
              |"currencyPair":"FOO-BAR",
              |"triggeredBy": {"kind":"trend-change-detection", "source": "close", "transformation": {"kind": "hma", "length": 16}},
              |"condition": {"kind":"trend-direction-change", "from":"downward", "to":"upward"}
              |}""".stripMargin)
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.UnprocessableEntity, Some("""{"message":"FOO-BAR is not valid currency pair representation"}"""))
        verifyNoInteractions(svc)
      }
    }

    "GET /signals" should {
      "return all submitted signals" in {
        val svc = mock[SignalService[IO]]
        when(svc.getAll(any[UserId], any[SearchParams])).thenReturnIO(List(Signals.trendDirectionChanged))

        val req = Request[IO](Method.GET, uri"/signals?from=2020-01-01&to=2021-01-01T04:01:00Z&currencyPair=GBP/USD").withAuthHeader()
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = s"""[{
               |"interval":"H1",
               |"currencyPair":"GBPEUR",
               |"time": "${Signals.ts}",
               |"triggeredBy": {"kind":"trend-change-detection", "source": "close", "transformation": {"kind": "hma", "length": 16}},
               |"condition": {"kind":"trend-direction-change","from":"downward","to":"upward","previousTrendLength":1}
               |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAll(
          Users.uid,
          SearchParams(Some(Instant.parse("2020-01-01T00:00:00Z")), Some(Instant.parse("2021-01-01T04:01:00Z")), Some(Markets.gbpusd))
        )
      }

      "return error when date 'from' is after 'to'" in {
        val svc = mock[SignalService[IO]]

        val req = Request[IO](Method.GET, uri"/signals?from=2021-01-02&to=2021-01-01").withAuthHeader()
        val res = SignalController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = s"""{"message":"Date 'from' must be before date 'to'"}"""
        res mustHaveStatus (Status.UnprocessableEntity, Some(responseBody))
        verifyNoInteractions(svc)
      }
    }
  }
}
