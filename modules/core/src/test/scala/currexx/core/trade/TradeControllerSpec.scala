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
             |    "price" : 3.0,
             |    "volume" : 0.1,
             |    "currencyPair" : "GBPEUR"
             |  },
             |  "broker" : {
             |    "apiKey" : "key",
             |    "accountId" : "account",
             |    "demo" : true,
             |    "broker" : "oanda"
             |  },
             |  "time" : "${Trades.ts}"
             |}
             |]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAllOrders(Users.uid, SearchParams(Some(Instant.parse("2020-01-01T00:00:00Z")), None, Some(Markets.gbpeur)))
      }
    }

    "GET /trade/orders/stats" should {
      "return order statistics" in {
        val svc = mock[TradeService[IO]]
        val stats = OrderStatistics(
          totalOrders = 10,
          successfulOrders = 7,
          pendingOrders = 2,
          cancelledOrders = 1,
          noPositionOrders = 0,
          enterOrders = EnterOrderStats(
            total = 6,
            buyCount = 4,
            sellCount = 2,
            totalVolume = BigDecimal(0.6),
            averageVolume = Some(BigDecimal(0.1))
          ),
          exitOrders = 4,
          currencyBreakdown = List(
            CurrencyStatistics(
              currencyPair = Markets.gbpeur,
              totalOrders = 10,
              successfulOrders = 7,
              pendingOrders = 2,
              cancelledOrders = 1,
              noPositionOrders = 0,
              enterOrders = 6,
              exitOrders = 4,
              buyOrders = 4,
              sellOrders = 2,
              totalVolume = BigDecimal(0.6)
            )
          )
        )
        when(svc.getOrderStatistics(any[UserId], any[SearchParams])).thenReturnIO(stats)

        val req = Request[IO](Method.GET, uri"/trade/orders/stats?from=2020-01-01&to=2020-12-31&currencyPair=GBPEUR").withAuthHeader()
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          """{
            |  "totalOrders" : 10,
            |  "successfulOrders" : 7,
            |  "pendingOrders" : 2,
            |  "cancelledOrders" : 1,
            |  "noPositionOrders" : 0,
            |  "enterOrders" : {
            |    "total" : 6,
            |    "buyCount" : 4,
            |    "sellCount" : 2,
            |    "totalVolume" : 0.6,
            |    "averageVolume" : 0.1
            |  },
            |  "exitOrders" : 4,
            |  "currencyBreakdown" : [
            |    {
            |      "currencyPair" : "GBPEUR",
            |      "totalOrders" : 10,
            |      "successfulOrders" : 7,
            |      "pendingOrders" : 2,
            |      "cancelledOrders" : 1,
            |      "noPositionOrders" : 0,
            |      "enterOrders" : 6,
            |      "exitOrders" : 4,
            |      "buyOrders" : 4,
            |      "sellOrders" : 2,
            |      "totalVolume" : 0.6
            |    }
            |  ]
            |}""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        
        verify(svc).getOrderStatistics(
          Users.uid,
          SearchParams(
            Some(Instant.parse("2020-01-01T00:00:00Z")),
            Some(Instant.parse("2020-12-31T00:00:00Z")),
            Some(Markets.gbpeur)
          )
        )
      }

      "return statistics without date filters" in {
        val svc = mock[TradeService[IO]]
        val stats = OrderStatistics(
          totalOrders = 5,
          successfulOrders = 4,
          pendingOrders = 1,
          cancelledOrders = 0,
          noPositionOrders = 0,
          enterOrders = EnterOrderStats(
            total = 3,
            buyCount = 2,
            sellCount = 1,
            totalVolume = BigDecimal(0.3),
            averageVolume = Some(BigDecimal(0.1))
          ),
          exitOrders = 2,
          currencyBreakdown = List.empty
        )
        when(svc.getOrderStatistics(any[UserId], any[SearchParams])).thenReturnIO(stats)

        val req = Request[IO](Method.GET, uri"/trade/orders/stats").withAuthHeader()
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          """{
            |  "totalOrders" : 5,
            |  "successfulOrders" : 4,
            |  "pendingOrders" : 1,
            |  "cancelledOrders" : 0,
            |  "noPositionOrders" : 0,
            |  "enterOrders" : {
            |    "total" : 3,
            |    "buyCount" : 2,
            |    "sellCount" : 1,
            |    "totalVolume" : 0.3,
            |    "averageVolume" : 0.1
            |  },
            |  "exitOrders" : 2,
            |  "currencyBreakdown" : [
            |  ]
            |}""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getOrderStatistics(Users.uid, SearchParams(None, None, None))
      }
      
      "return bad request when from date is after to date" in {
        val svc = mock[TradeService[IO]]

        val req = Request[IO](Method.GET, uri"/trade/orders/stats?from=2020-12-31&to=2020-01-01").withAuthHeader()
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = """{"message":"Date 'from' must be before date 'to'"}"""
        res mustHaveStatus (Status.UnprocessableContent, Some(responseBody))
        verifyNoInteractions(svc)
      }
    }
  }
}
