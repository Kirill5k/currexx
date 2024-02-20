package currexx.core.common

import cats.effect.IO
import config.AppConfig
import currexx.domain.IOWordSpec

class AppConfigSpec extends IOWordSpec {

  System.setProperty("MONGO_HOST", "mongo")
  System.setProperty("MONGO_USER", "user")
  System.setProperty("MONGO_PASSWORD", "password")
  System.setProperty("ALPHA_VANTAGE_API_KEY", "av-key")
  System.setProperty("TWELVE_DATA_API_KEY", "td-key")

  "An AppConfig" should {

    "load itself from application.conf" in {
      AppConfig.load[IO].asserting { config =>
        config.server.host mustBe "0.0.0.0"
        config.mongo.connectionUri mustBe "mongodb+srv://user:password@mongo/currexx"
        config.clients.alphaVantage.apiKey mustBe "av-key"
        config.clients.twelveData.apiKey mustBe "td-key"
        config.clients.xtb.baseUri mustBe "wss://ws.xtb.com"
      }
    }
  }
}
