package currexx.core.trade

import cats.effect.IO
import currexx.core.auth.Authenticator
import currexx.core.common.http.SearchParams
import currexx.core.{CatsSpec, ControllerSpec}
import currexx.core.fixtures.{Markets, Sessions, Trades, Users}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.user.UserId
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

import java.time.Instant

class TradeControllerSpec extends ControllerSpec {

  "A TradeController" when {
    given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "POST /trade/orders" should {
      "submit order placement request" in {
        val svc = mock[TradeService[IO]]
        when(svc.placeOrder(any[UserId], any[CurrencyPair], any[TradeOrder], any[Boolean])).thenReturn(IO.unit)

        val requestBody =
          s"""
             |{
             |  "currencyPair" : "GBP/EUR",
             |  "order" : {
             |    "kind" : "enter",
             |    "position" : "buy",
             |    "volume" : 0.1,
             |    "stopLoss" : 25,
             |    "trailingStopLoss" : null,
             |    "takeProfit" : null
             |  }
             |}
             |""".stripMargin

        val req = requestWithAuthHeader(uri"/trade/orders?closePendingOrders=false", Method.POST)
          .withEntity(parseJson(requestBody))
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.Created, None)
        verify(svc).placeOrder(Users.uid, Markets.gbpeur, Trades.order.order, false)
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
             |  "currencyPair" : "GBPEUR",
             |  "order" : {
             |    "kind" : "enter",
             |    "position" : "buy",
             |    "volume" : 0.1,
             |    "stopLoss" : 25,
             |    "trailingStopLoss" : null,
             |    "takeProfit" : null
             |  },
             |  "broker" : {
             |    "broker" : "vindaloo",
             |    "externalId" : "1"
             |  },
             |  "currentPrice" : {
             |    "open" : 2.0,
             |    "high" : 4.0,
             |    "low" : 1.0,
             |    "close" : 3.0,
             |    "volume" : 1000,
             |    "time" : "${Markets.ts}"
             |  },
             |  "time" : "${Trades.ts}"
             |}
             |]""".stripMargin
        verifyJsonResponse(res, Status.Ok, Some(responseBody))
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
        verifyJsonResponse(res, Status.Ok, Some(responseBody))
        verify(svc).getSettings(Users.uid)
      }

      "return error when signals are not configured in the given accounts" in {
        val svc = mock[TradeService[IO]]
        when(svc.getSettings(any[UserId])).thenReturn(IO.raiseError(AppError.NotSetup("Trade")))

        val req = requestWithAuthHeader(uri"/trade/settings", Method.GET)
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NotFound, Some("""{"message":"Current account doesn't have Trade-settings set up"}"""))
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

        val req = requestWithAuthHeader(uri"/trade/settings", Method.PUT).withEntity(parseJson(requestBody))
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).updateSettings(Trades.settings.copy(strategy = TradeStrategy.TrendChange))
      }
    }
  }
}
