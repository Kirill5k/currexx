package currexx.core.trade

import cats.effect.IO
import currexx.core.auth.Authenticator
import currexx.core.common.http.SearchParams
import currexx.core.{ControllerSpec, IOWordSpec}
import currexx.core.fixtures.{Markets, Sessions, Trades, Users}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.user.UserId
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

import java.time.Instant

class TradeControllerSpec extends ControllerSpec {

  "A TradeController" when {
    given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "POST /trade/orders" should {
      "submit order placement request" in {
        val svc = mock[TradeService[IO]]
        when(svc.placeOrder(any[UserId], any[TradeOrder], any[Boolean])).thenReturn(IO.unit)

        val requestBody =
          s"""
             |{
             |  "kind" : "enter",
             |  "currencyPair" : "GBP/EUR",
             |  "position" : "buy",
             |  "volume" : 0.1,
             |  "price" : 3.0
             |}
             |""".stripMargin

        val req = requestWithAuthHeader(uri"/trade/orders?closePendingOrders=false", Method.POST).withJsonBody(parseJson(requestBody))
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Created, None)
        verify(svc).placeOrder(Users.uid, Trades.order.order, false)
      }
    }

    "DELETE /trade/orders" should {
      "close all current positions" in {
        val svc = mock[TradeService[IO]]
        when(svc.closeOpenOrders(any[UserId])).thenReturn(IO.unit)

        val req = requestWithAuthHeader(uri"/trade/orders", Method.DELETE)
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).closeOpenOrders(Users.uid)
      }

      "close all for passed currency pair" in {
        val svc = mock[TradeService[IO]]
        when(svc.closeOpenOrders(any[UserId], any[CurrencyPair])).thenReturn(IO.unit)

        val req = requestWithAuthHeader(uri"/trade/orders?currencyPair=GBPEUR", Method.DELETE)
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).closeOpenOrders(Users.uid, Markets.gbpeur)
      }
    }

    "GET /trade/orders" should {
      "return placed orders" in {
        val svc = mock[TradeService[IO]]
        when(svc.getAllOrders(any[UserId], any[SearchParams])).thenReturn(IO.pure(List(Trades.order)))

        val req = requestWithAuthHeader(uri"/trade/orders?from=2020-01-01&currencyPair=GBP/EUR", Method.GET)
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

    "GET /trade/settings" should {
      "return trade settings with broker and trading parameters" in {
        val svc = mock[TradeService[IO]]
        when(svc.getSettings(any[UserId])).thenReturn(IO.pure(Trades.settings))

        val req = requestWithAuthHeader(uri"/trade/settings", Method.GET)
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""{
             |"strategy": "disabled",
             |"comment": "test",
             |"broker" : {
             |  "broker" : "vindaloo",
             |  "externalId" : "1"
             |},
             |"trading" : {
             |  "volume" : 0.1,
             |  "stopLoss" : null,
             |  "stopLossPerCurrency" : {},
             |  "trailingStopLoss" : null,
             |  "takeProfit" : null
             |}
             |}""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getSettings(Users.uid)
      }

      "return error when signals are not configured in the given accounts" in {
        val svc = mock[TradeService[IO]]
        when(svc.getSettings(any[UserId])).thenReturn(IO.raiseError(AppError.NotSetup("Trade")))

        val req = requestWithAuthHeader(uri"/trade/settings", Method.GET)
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NotFound, Some("""{"message":"Current account doesn't have Trade-settings set up"}"""))
        verify(svc).getSettings(Users.uid)
      }
    }

    "PUT /trade/settings" should {
      "update trade settings" in {
        val svc = mock[TradeService[IO]]
        when(svc.updateSettings(any[TradeSettings])).thenReturn(IO.unit)

        val requestBody =
          s"""{
             |"strategy": "trend-change",
             |"comment": "test",
             |"broker" : {
             |  "broker" : "vindaloo",
             |  "externalId" : "1"
             |},
             |"trading" : {
             |  "volume" : 0.1,
             |  "stopLoss" : null,
             |  "stopLossPerCurrency" : {},
             |  "trailingStopLoss" : null,
             |  "takeProfit" : null
             |}
             |}""".stripMargin

        val req = requestWithAuthHeader(uri"/trade/settings", Method.PUT).withJsonBody(parseJson(requestBody))
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).updateSettings(Trades.settings.copy(strategy = TradeStrategy.TrendChange))
      }
    }
  }
}
