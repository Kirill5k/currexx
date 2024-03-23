package currexx.core.trade

import cats.effect.IO
import currexx.core.auth.Authenticator
import currexx.core.common.http.SearchParams
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import currexx.core.fixtures.{Markets, Sessions, Trades, Users}
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.user.UserId
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

import java.time.Instant

class TradeControllerSpec extends HttpRoutesWordSpec {

  "A TradeController" when {
    given Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "POST /trade/orders" should {
      "submit order placement request" in {
        val svc = mock[TradeService[IO]]
        when(svc.placeOrder(any[UserId], any[TradeOrder], any[Boolean])).thenReturn(IO.unit)

        val requestBody = """
             |{
             |  "kind" : "enter",
             |  "currencyPair" : "GBP/EUR",
             |  "position" : "buy",
             |  "volume" : 0.1,
             |  "price" : 3.0
             |}
             |""".stripMargin

        val req = Request[IO](Method.POST, uri"/trade/orders?closePendingOrders=false")
          .withAuthHeader()
          .withBody(requestBody)
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Created, None)
        verify(svc).placeOrder(Users.uid, Trades.order.order, false)
      }
    }

    "DELETE /trade/orders" should {
      "close all current positions" in {
        val svc = mock[TradeService[IO]]
        when(svc.closeOpenOrders(any[UserId])).thenReturn(IO.unit)

        val req = Request[IO](Method.DELETE, uri"/trade/orders").withAuthHeader()
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).closeOpenOrders(Users.uid)
      }

      "close all for passed currency pair" in {
        val svc = mock[TradeService[IO]]
        when(svc.closeOpenOrders(any[UserId], any[CurrencyPair])).thenReturn(IO.unit)

        val req = Request[IO](Method.DELETE, uri"/trade/orders?currencyPair=GBPEUR").withAuthHeader()
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).closeOpenOrders(Users.uid, Markets.gbpeur)
      }
    }

    "GET /trade/orders" should {
      "return placed orders" in {
        val svc = mock[TradeService[IO]]
        when(svc.getAllOrders(any[UserId], any[SearchParams])).thenReturnIO(List(Trades.order))

        val req = Request[IO](Method.GET, uri"/trade/orders?from=2020-01-01&currencyPair=GBP/EUR").withAuthHeader()
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""[
             |{
             |  "order" : {
             |    "kind" : "enter",
             |    "position" : "buy",
             |    "volume" : 0.1,
             |    "price" : 3.0,
             |    "currencyPair" : "GBPEUR"
             |  },
             |  "broker" : {
             |    "broker" : "vindaloo",
             |    "externalId" : "1"
             |  },
             |  "time" : "${Trades.ts}"
             |}
             |]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAllOrders(Users.uid, SearchParams(Some(Instant.parse("2020-01-01T00:00:00Z")), None, Some(Markets.gbpeur)))
      }
    }
  }
}
