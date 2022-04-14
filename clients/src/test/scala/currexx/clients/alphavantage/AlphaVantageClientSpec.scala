package currexx.clients.alphavantage

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.clients.{ApiClientSpec, MarketDataClientConfig}
import currexx.domain.market.{CurrencyPair, PriceRange, Interval}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import squants.market.{GBP, USD}
import sttp.client3.{Response, SttpBackend}

import java.time.Instant

class AlphaVantageClientSpec extends ApiClientSpec {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val config = MarketDataClientConfig("http://alpha-vantage.com", "api-key")
  val pair   = CurrencyPair(GBP, USD)

  "An AlphaVantageClient" should {
    "retrieve hourly price time series data" in {
      val params =
        Map("function" -> "FX_INTRADAY", "from_symbol" -> "GBP", "to_symbol" -> "USD", "apikey" -> "api-key", "interval" -> "60min")
      val testingBackend: SttpBackend[IO, Any] = backendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("alpha-vantage.com/query") && r.hasParams(params) =>
            Response.ok(json("alphavantage/gbp-usd-hourly-prices-response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        client <- AlphaVantageClient.make[IO](config, testingBackend)
        res    <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.unsafeToFuture().map { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        timeSeriesData.prices must have size 100

        timeSeriesData.prices.head mustBe PriceRange(
          BigDecimal(1.30370),
          BigDecimal(1.30505),
          BigDecimal(1.30320),
          BigDecimal(1.30438),
          Instant.parse("2022-04-14T15:00:00Z")
        )

        timeSeriesData.prices.last mustBe PriceRange(
          BigDecimal(1.30425),
          BigDecimal(1.30522),
          BigDecimal(1.30360),
          BigDecimal(1.30418),
          Instant.parse("2022-04-08T11:00:00Z")
        )
      }
    }

    "retrieve daily price time series data" in {
      val params = Map("function" -> "FX_DAILY", "from_symbol" -> "GBP", "to_symbol" -> "USD", "apikey" -> "api-key")
      val testingBackend: SttpBackend[IO, Any] = backendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("alpha-vantage.com/query") && r.hasParams(params) =>
            Response.ok(json("alphavantage/gbp-usd-daily-prices-response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        client <- AlphaVantageClient.make[IO](config, testingBackend)
        res    <- client.timeSeriesData(pair, Interval.D1)
      yield res

      result.unsafeToFuture().map { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.D1
        timeSeriesData.prices must have size 100

        timeSeriesData.prices.head mustBe PriceRange(
          BigDecimal(1.31211),
          BigDecimal(1.31472),
          BigDecimal(1.30320),
          BigDecimal(1.30395),
          Instant.parse("2022-04-14T00:00:00Z")
        )

        timeSeriesData.prices.last mustBe PriceRange(
          BigDecimal(1.33175),
          BigDecimal(1.33657),
          BigDecimal(1.32770),
          BigDecimal(1.33307),
          Instant.parse("2021-11-26T00:00:00Z")
        )
      }
    }
  }
}
