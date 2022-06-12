package currexx.core.trade

import cats.effect.IO
import currexx.core.auth.Authenticator
import currexx.core.{CatsSpec, ControllerSpec}
import currexx.core.fixtures.{Markets, Sessions, Trades, Users}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

class TradeControllerSpec extends ControllerSpec {

  "A TradeController" when {
    given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "GET /trade/orders" should {
      "return placed orders" in {
        val svc = mock[TradeService[IO]]
        when(svc.getAllOrders(any[UserId])).thenReturn(IO.pure(List(Trades.order)))

        val req = requestWithAuthHeader(uri"/trade/orders", Method.GET)
        val res = TradeController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""[
             |{
             |  "currencyPair" : "GBP/EUR",
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
        verify(svc).getAllOrders(Users.uid)
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
