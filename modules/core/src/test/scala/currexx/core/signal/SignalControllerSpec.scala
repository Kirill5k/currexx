package currexx.core.signal

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import currexx.core.auth.Authenticator
import currexx.core.common.http.SearchParams
import currexx.core.fixtures.{Markets, Sessions, Signals, Users}
import currexx.domain.signal.Direction
import currexx.domain.user.UserId
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

import java.time.Instant

class SignalControllerSpec extends HttpRoutesWordSpec {

  "A SignalController" when {
    given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "GET /signals" should {
      "return all stored signals" in {
        val svc = mock[SignalService[IO]]
        when(svc.getAll(any[UserId], any[SearchParams])).thenReturnIO(List(Signals.trend(Direction.Upward)))

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
        res mustHaveStatus (Status.UnprocessableContent, Some(responseBody))
        verifyNoInteractions(svc)
      }
    }
  }
}
