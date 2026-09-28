package currexx.core.common

import cats.effect.IO
import config.AppConfig
import kirill5k.common.cats.test.IOWordSpec
import pureconfig.ConfigSource

class AppConfigSpec extends IOWordSpec {

  System.setProperty("MONGO_HOST", "mongo")
  System.setProperty("MONGO_USER", "user")
  System.setProperty("MONGO_PASSWORD", "password")
  System.setProperty("OANDA_DATA_API_KEY", "od-key")
  System.setProperty("ALPHA_VANTAGE_API_KEY", "av-key")
  System.setProperty("TWELVE_DATA_API_KEY", "td-key")

  "An AppConfig" should {

    "load itself from application.conf" in
      AppConfig.load[IO].asserting { config =>
        config.server.host mustBe "0.0.0.0"
        config.mongo.user mustBe "user"
        config.clients.alphaVantage.apiKey mustBe "av-key"
        config.clients.twelveData.apiKey mustBe "td-key"
        config.clients.twelveData.fetchCandleCount mustBe 150
        config.clients.twelveData.signalCandleCount mustBe 100
        config.clients.oandaData.apiKey mustBe "od-key"
        config.clients.oandaData.fetchCandleCount mustBe 150
        config.clients.oandaData.signalCandleCount mustBe 100
        config.auth.passwordSalt.value mustBe "$2a$10$8K1p/a0dL1LXMIgoEDFrwO"
        config.auth.passwordSalt.toString mustBe "<SECRET>"
        config.auth.jwt.alg mustBe "HS256"
        config.auth.jwt.secret.value mustBe "secret-key"
        config.auth.jwt.secret.toString mustBe "<SECRET>"
      }

    "load custom candle counts for each market data provider" in
      IO.blocking {
        ConfigSource
          .string("""
            clients {
              oanda-data {
                fetch-candle-count = 120
                signal-candle-count = 80
              }
              twelve-data {
                fetch-candle-count = 180
                signal-candle-count = 120
              }
            }
          """)
          .withFallback(ConfigSource.default)
          .loadOrThrow[AppConfig]
      }.asserting { config =>
        config.clients.oandaData.fetchCandleCount mustBe 120
        config.clients.oandaData.signalCandleCount mustBe 80
        config.clients.twelveData.fetchCandleCount mustBe 180
        config.clients.twelveData.signalCandleCount mustBe 120
      }
  }
}
