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

    "GET /market/state" should {
      "return state of the traded currencies" in {
        val svc = mock[MarketService[IO]]
        when(svc.getState(any[UserId])).thenReturn(IO.pure(List(Markets.stateWithSignal)))

        val req = requestWithAuthHeader(uri"/market/state", Method.GET)
        val res = MarketController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""{
             |"GBP/EUR" : {
             |  "currentPosition" : "buy",
             |  "latestPrice" : {
             |    "open" : 2.0,
             |    "high" : 4.0,
             |    "low" : 1.0,
             |    "close" : 3.0,
             |    "volume" : 1000,
             |    "time" : "${Markets.ts}"
             |  },
             |  "signals" : {
             |    "macd" : [
             |      {
             |        "condition" : {
             |          "kind" : "crossing-up"
             |        },
             |        "time" : "${Markets.ts}"
             |      }
             |    ]
             |  },
             |  "lastUpdatedAt" : "${Markets.ts}"
             |}
             |}""".stripMargin

        verifyJsonResponse(res, Status.Ok, Some(responseBody))
        verify(svc).getState(Users.uid)
      }
    }
  }
}
