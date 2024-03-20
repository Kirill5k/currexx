package currexx.clients.data.twelvedata

import cats.effect.IO
import currexx.domain.market.Currency.{EUR, USD}
import currexx.domain.market.{CurrencyPair, Interval, PriceRange}
import kirill5k.common.sttp.test.SttpWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client3.{Response, SttpBackend}

import java.time.{Instant, LocalTime}
import scala.concurrent.duration.*

class TwelveDataClientSpec extends SttpWordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val config = TwelveDataConfig("http://twelve-data.com", "api-key")
  val pair   = CurrencyPair(EUR, USD)

  "A TwelveDataClient" should {
    "retrieve time-series data" in {
      val requestParams = Map(
        "symbol"     -> "EUR/USD",
        "interval"   -> "1day",
        "apikey"     -> "api-key",
        "outputsize" -> "150"
      )

      val testingBackend: SttpBackend[IO, Any] = backendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
            Response.ok(readJson("twelvedata/eur-usd-daily-prices.response.json"))
          case _ => throw new RuntimeException()
        }
      val result = for
        client <- TwelveDataClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.D1)
      yield res

      result.asserting { timeSeriesData =>
        val currentTime = LocalTime.now().toString.slice(0, 5)
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.D1
        timeSeriesData.prices must have size 150
        timeSeriesData.prices.head mustBe PriceRange(1.0027, 1.0032, 0.9973, 1.0005, 0.0, Instant.parse(s"2022-11-08T${currentTime}:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.05475, 1.055, 1.05405, 1.0547, 0.0, Instant.parse("2022-05-07T00:00:00Z"))
      }
    }

    "retry after some delay in case of api limit error" in {
      val testingBackend: SttpBackend[IO, Any] = backendStub.whenAnyRequest
        .thenRespondCyclicResponses(
          Response.ok(readJson("twelvedata/limit-error.json")),
          Response.ok(readJson("twelvedata/eur-usd-daily-prices.response.json"))
        )
      val result = for
        client <- TwelveDataClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.D1)
      yield res

      result.asserting { timeSeriesData =>
        val currentTime = LocalTime.now().toString.slice(0, 5)
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.D1
        timeSeriesData.prices must have size 150
        timeSeriesData.prices.head mustBe PriceRange(1.0027, 1.0032, 0.9973, 1.0005, 0.0, Instant.parse(s"2022-11-08T${currentTime}:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.05475, 1.055, 1.05405, 1.0547, 0.0, Instant.parse("2022-05-07T00:00:00Z"))
      }
    }
  }
}
