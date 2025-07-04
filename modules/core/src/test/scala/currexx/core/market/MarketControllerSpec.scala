package currexx.core.market

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import currexx.core.auth.Authenticator
import currexx.core.fixtures.{Markets, Sessions, Users}
import currexx.domain.market.CurrencyPair
import currexx.domain.user.UserId
import org.http4s.implicits.*
import org.http4s.{Method, Status, Uri, Request}

class MarketControllerSpec extends HttpRoutesWordSpec {

  "A MarketController" when {
    given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "DELETE /market/state" should {
      "clear state of traded currencies and close pending orders by default" in {
        val svc = mock[MarketService[IO]]
        when(svc.clearState(any[UserId], any[Boolean])).thenReturn(IO.unit)

        val req = Request[IO](Method.DELETE, uri"/market/state?dryRun=false").withAuthHeader()
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).clearState(Users.uid, true)
      }

      "clear state of traded currencies without closing pending orders" in {
        val svc = mock[MarketService[IO]]
        when(svc.clearState(any[UserId], any[Boolean])).thenReturn(IO.unit)

        val req = Request[IO](Method.DELETE, uri"/market/state?closePendingOrders=false&dryRun=false").withAuthHeader()
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).clearState(Users.uid, false)
      }

      "clear state of a single currency" in {
        val svc = mock[MarketService[IO]]
        when(svc.clearState(any[UserId], any[CurrencyPair], any[Boolean])).thenReturn(IO.unit)

        val req = Request[IO](Method.DELETE, uri"/market/state?closePendingOrders=false&dryRun=false&currencyPair=GBPEUR").withAuthHeader()
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).clearState(Users.uid, Markets.gbpeur, false)
      }

      "not do anything when dryRun is not provided" in {
        val svc = mock[MarketService[IO]]

        val req = Request[IO](Method.DELETE, uri"/market/state").withAuthHeader()
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verifyNoInteractions(svc)
      }
    }

    "GET /market/state" should {
      "return state of traded currencies" in {
        val svc = mock[MarketService[IO]]
        when(svc.getState(any[UserId])).thenReturn(IO.pure(List(Markets.state)))

        val req = Request[IO](Method.GET, uri"/market/state").withAuthHeader()
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""
             |{
             |  "GBPEUR": {
             |    "currentPosition": {
             |      "position": "buy",
             |      "openedAt": "${Markets.ts}",
             |      "openPrice": 3
             |    },
             |    "profile" : {
             |      "trend" : {
             |        "direction" : "upward",
             |        "confirmedAt" : "${Markets.ts}"
             |      },
             |      "crossover" : null,
             |      "momentum" : null,
             |      "lastMomentumValue" : null,
             |      "volatility" : null,
             |      "lastVolatilityValue" : null
             |    },
             |    "lastUpdatedAt": "${Markets.ts}",
             |    "createdAt": "${Markets.ts}"
             |  }
             |}
             |""".stripMargin

        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getState(Users.uid)
      }
    }
  }
}
