package currexx.core.settings

import cats.effect.IO
import currexx.core.auth.Authenticator
import currexx.core.fixtures.{Sessions, Users}
import currexx.core.settings.SettingsController.GlobalSettingsView
import currexx.domain.user.UserId
import io.circe.parser.decode
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

class SettingsControllerSpec extends HttpRoutesWordSpec {

  val fullSettingsJson: String =
    """{
      |  "signal" : {
      |    "indicators" : [
      |      {
      |        "source" : "hlc3",
      |        "line1Transformation" : {
      |          "length" : 43,
      |          "phase" : -67,
      |          "power" : 1,
      |          "kind" : "jma"
      |        },
      |        "line2Transformation" : {
      |          "length" : 16,
      |          "phase" : 35,
      |          "power" : 8,
      |          "kind" : "jma"
      |        },
      |        "kind" : "lines-crossing"
      |      },
      |      {
      |        "source" : "close",
      |        "transformation" : {
      |          "length" : 16,
      |          "kind" : "rsx"
      |        },
      |        "upperBoundary" : 51.0,
      |        "lowerBoundary" : 44.0,
      |        "kind" : "threshold-crossing"
      |      },
      |      {
      |        "atrLength" : 6,
      |        "smoothingType" : {
      |          "length" : 4,
      |          "kind" : "sma"
      |        },
      |        "kind" : "volatility-regime-detection"
      |      }
      |    ]
      |  },
      |  "trade" : {
      |    "strategy" : {
      |      "openRules" : [
      |        {
      |          "action" : "open-long",
      |          "conditions" : {
      |            "conditions" : [
      |              {
      |                "direction" : "upward",
      |                "kind" : "crossover-occurred"
      |              },
      |              {
      |                "regime" : "low",
      |                "kind" : "volatility-is"
      |              },
      |              {
      |                "zone" : "neutral",
      |                "kind" : "momentum-is-in"
      |              }
      |            ],
      |            "kind" : "all-of"
      |          }
      |        },
      |        {
      |          "action" : "open-short",
      |          "conditions" : {
      |            "conditions" : [
      |              {
      |                "direction" : "downward",
      |                "kind" : "crossover-occurred"
      |              },
      |              {
      |                "regime" : "low",
      |                "kind" : "volatility-is"
      |              },
      |              {
      |                "zone" : "neutral",
      |                "kind" : "momentum-is-in"
      |              }
      |            ],
      |            "kind" : "all-of"
      |          }
      |        }
      |      ],
      |      "closeRules" : [
      |        {
      |          "action" : "close-position",
      |          "conditions" : {
      |            "conditions" : [
      |              {
      |                "conditions" : [
      |                  {
      |                    "position" : "buy",
      |                    "kind" : "position-is"
      |                  },
      |                  {
      |                    "zone" : "overbought",
      |                    "kind" : "momentum-entered"
      |                  }
      |                ],
      |                "kind" : "all-of"
      |              },
      |              {
      |                "conditions" : [
      |                  {
      |                    "position" : "sell",
      |                    "kind" : "position-is"
      |                  },
      |                  {
      |                    "zone" : "oversold",
      |                    "kind" : "momentum-entered"
      |                  }
      |                ],
      |                "kind" : "all-of"
      |              }
      |            ],
      |            "kind" : "any-of"
      |          }
      |        }
      |      ]
      |    },
      |    "broker" : {
      |      "broker" : "oanda",
      |      "demo" : true,
      |      "accountId" : "101",
      |      "apiKey" : "secret-key"
      |    },
      |    "trading" : {
      |      "volume" : 0.1,
      |      "stopLoss" : null,
      |      "stopLossPerCurrency" : {},
      |      "trailingStopLoss" : null,
      |      "takeProfit" : null
      |    }
      |  },
      |  "note" : "test account"
      |}""".stripMargin

  "A SettingsController" when {
    "GET /settings" should {
      "return empty global settings" in {
        val svc = mock[SettingsService[IO]]
        when(svc.get(any[UserId])).thenReturnIO(GlobalSettings(userId = Users.uid, signal = None, trade = None, note = None))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = Request[IO](Method.GET, uri"/settings").withAuthHeader()
        val res = SettingsController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""{
             |  "signal" : null,
             |  "trade" : null,
             |  "note" : null
             |}""".stripMargin

        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).get(Users.uid)
      }

      "return full global settings with signal and trade configuration" in {
        val svc = mock[SettingsService[IO]]
        val settings = decode[GlobalSettingsView](fullSettingsJson).fold(throw _, _.toDomain(Users.uid))
        when(svc.get(any[UserId])).thenReturnIO(settings)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = Request[IO](Method.GET, uri"/settings").withAuthHeader()
        val res = SettingsController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Ok, Some(fullSettingsJson))
        verify(svc).get(Users.uid)
      }
    }

    "PUT /settings" should {
      "update settings and return 204 when request is valid" in {
        val svc = mock[SettingsService[IO]]
        when(svc.update(any[GlobalSettings])).thenReturnIO(())

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = Request[IO](Method.PUT, uri"/settings").withAuthHeader().withBody(fullSettingsJson)
        val res = SettingsController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).update(any[GlobalSettings])
      }

      "return 422 when request body is invalid" in {
        val svc = mock[SettingsService[IO]]

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = Request[IO](Method.PUT, uri"/settings").withAuthHeader().withBody(
          """{
            |"signal": "invalid",
            |"trade": {
            |        "strategy": {
            |            "openRules": [],
            |            "closeRules": []
            |        },
            |        "broker": {
            |            "apiKey": "apiKey",
            |            "demo": true,
            |            "accountId": "1",
            |            "broker": "oanda"
            |        },
            |        "trading": {
            |
            |        }
            |    }
            |}""".stripMargin)
        val res = SettingsController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.UnprocessableContent, Some("""{"message" : "Got value '\"invalid\"' with wrong type, expecting object, Missing required field trade.trading.volume, Missing required field trade.trading.stopLossPerCurrency"}"""))
        verifyNoInteractions(svc)
      }
    }
  }
}
