package currexx.core.market

import cats.effect.IO
import currexx.core.ControllerSpec
import currexx.core.auth.Authenticator
import currexx.core.fixtures.{Markets, Sessions, Signals, Users}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

class MarketControllerSpec extends ControllerSpec {

  "A MarketController" when {
    given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "DELETE /market/state" should {
      "clear state of traded currencies and close pending orders by default" in {
        val svc = mock[MarketService[IO]]
        when(svc.clearState(any[UserId], any[Boolean])).thenReturn(IO.unit)

        val req = requestWithAuthHeader(uri"/market/state", Method.DELETE)
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).clearState(Users.uid, true)
      }

      "clear state of traded currencies without closing pending orders" in {
        val svc = mock[MarketService[IO]]
        when(svc.clearState(any[UserId], any[Boolean])).thenReturn(IO.unit)

        val req = requestWithAuthHeader(uri"/market/state?closePendingOrders=false", Method.DELETE)
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).clearState(Users.uid, false)
      }
    }

    "GET /market/state" should {
      "return state of traded currencies" in {
        val svc = mock[MarketService[IO]]
        when(svc.getState(any[UserId])).thenReturn(IO.pure(List(Markets.stateWithSignal)))

        val req = requestWithAuthHeader(uri"/market/state", Method.GET)
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""
             |{
             |  "GBP/EUR": {
             |    "currentPosition": {
             |      "position": "buy",
             |      "openedAt": "${Markets.ts}",
             |      "price": {
             |        "open": 2,
             |        "high": 4,
             |        "low": 1,
             |        "close": 3,
             |        "volume": 1000,
             |        "time": "${Markets.ts}"
             |      }
             |    },
             |    "latestPrice": {
             |      "open": 2,
             |      "high": 4,
             |      "low": 1,
             |      "close": 3,
             |      "volume": 1000,
             |      "time": "${Markets.ts}"
             |    },
             |    "signals": {
             |      "trend-change-detection": [
             |        {
             |          "triggeredBy": {
             |            "kind": "trend-change-detection",
             |            "source": "close",
             |            "transformation": {
             |              "kind": "hma",
             |              "length": 16
             |            }
             |          },
             |          "condition": {
             |            "kind": "trend-direction-change",
             |            "from": "downward",
             |            "to": "upward",
             |            "previousTrendLength": 1
             |          },
             |          "time": "${Signals.ts}"
             |        }
             |      ]
             |    },
             |    "lastUpdatedAt": "${Markets.ts}"
             |  }
             |}
             |""".stripMargin

        verifyJsonResponse(res, Status.Ok, Some(responseBody))
        verify(svc).getState(Users.uid)
      }
    }
  }
}
