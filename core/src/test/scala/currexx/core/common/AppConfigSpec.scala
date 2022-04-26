package currexx.core.common

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import config.AppConfig
import currexx.core.CatsSpec

class AppConfigSpec extends CatsSpec {

  System.setProperty("MONGO_HOST", "mongo")
  System.setProperty("MONGO_USER", "user")
  System.setProperty("MONGO_PASSWORD", "password")
  System.setProperty("ALPHA_VANTAGE_API_KEY", "av-key")

  "An AppConfig" should {

    "load itself from reference.conf" in {
      AppConfig.load[IO].unsafeToFuture().map { config =>
        config.server.host mustBe "0.0.0.0"
        config.mongo.connectionUri mustBe "mongodb+srv://user:password@mongo/currexx"
        config.clients.alphaVantage.apiKey mustBe Some("av-key")
      }
    }
  }
}
