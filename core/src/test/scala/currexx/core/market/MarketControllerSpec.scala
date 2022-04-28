package currexx.core.market

import cats.effect.IO
import currexx.core.ControllerSpec
import currexx.core.auth.Authenticator
import currexx.core.fixtures.{Markets, Sessions, Users}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

class MarketControllerSpec extends ControllerSpec {

  "A MarketController" when {
    given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

    "GET /market/settings" should {
      "return market settings with broker and trading parameters" in {
        val svc = mock[MarketService[IO]]
        when(svc.getSettings(any[UserId])).thenReturn(IO.pure(Markets.settings))

        val req = requestWithAuthHeader(uri"/market/settings", Method.GET)
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""{
             |"broker" : {
             |  "broker" : "vindaloo",
             |  "externalId" : "1"
             |},
             |"trading" : {
             |  "volume" : 0.1,
             |  "stopLoss" : null,
             |  "trailingStopLoss" : null,
             |  "takeProfit" : null
             |}
             |}""".stripMargin
        verifyJsonResponse(res, Status.Ok, Some(responseBody))
        verify(svc).getSettings(Users.uid)
      }

      "return error when signals are not configured in the given accounts" in {
        val svc = mock[MarketService[IO]]
        when(svc.getSettings(any[UserId])).thenReturn(IO.raiseError(AppError.NotSetup("Market")))

        val req = requestWithAuthHeader(uri"/market/settings", Method.GET)
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NotFound, Some("""{"message":"Current account doesn't have Market-settings set up"}"""))
        verify(svc).getSettings(Users.uid)
      }
    }

    "PUT /market/settings" should {
      "update signal settings" in {
        val svc = mock[MarketService[IO]]
        when(svc.updateSettings(any[MarketSettings])).thenReturn(IO.unit)

        val requestBody =
          s"""{
             |"broker" : {
             |  "broker" : "vindaloo",
             |  "externalId" : "1"
             |},
             |"trading" : {
             |  "volume" : 0.1,
             |  "stopLoss" : null,
             |  "trailingStopLoss" : null,
             |  "takeProfit" : null
             |}
             |}""".stripMargin

        val req = requestWithAuthHeader(uri"/market/settings", Method.PUT).withEntity(parseJson(requestBody))
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).updateSettings(Markets.settings)
      }
    }
  }
}
