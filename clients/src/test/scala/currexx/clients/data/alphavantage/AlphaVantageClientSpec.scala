package currexx.clients.data.alphavantage

import cats.effect.IO
import currexx.clients.data.alphavantage.AlphaVantageClient
import currexx.clients.ClientSpec
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Interval, PriceRange}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import currexx.domain.market.Currency.{GBP, USD}
import sttp.client3.{Response, SttpBackend}
import sttp.model.StatusCode

import java.time.Instant
import scala.concurrent.duration.*

class AlphaVantageClientSpec extends ClientSpec {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val config = AlphaVantageConfig("http://alpha-vantage.com", "api-key")
  val pair   = CurrencyPair(GBP, USD)

  "An AlphaVantageClient" should {
    "retrieve hourly price time series data" in {
      val params = Map(
        "function"    -> "FX_INTRADAY",
        "from_symbol" -> "GBP",
        "to_symbol"   -> "USD",
        "apikey"      -> "api-key",
        "interval"    -> "60min"
      )
      val testingBackend: SttpBackend[IO, Any] = backendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("alpha-vantage.com/query") && r.hasParams(params) =>
            Response.ok(json("alphavantage/gbp-usd-hourly-prices-response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        client <- AlphaVantageClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        timeSeriesData.prices must have size 100
        timeSeriesData.prices.head mustBe PriceRange(1.30370, 1.30505, 1.30320, 1.30438, 0, Instant.parse("2022-04-14T15:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.30425, 1.30522, 1.30360, 1.30418, 0, Instant.parse("2022-04-08T11:00:00Z"))
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
        client <- AlphaVantageClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.D1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.D1
        timeSeriesData.prices must have size 100
        timeSeriesData.prices.head mustBe PriceRange(1.31211, 1.31472, 1.30320, 1.30395, 0, Instant.parse("2022-04-14T14:10:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.33175, 1.33657, 1.32770, 1.33307, 0, Instant.parse("2021-11-26T00:00:00Z"))
      }
    }

    "retrieve the latest price for a given currency pair" in {
      val testingBackend: SttpBackend[IO, Any] = backendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("alpha-vantage.com/query") =>
            Response.ok(json("alphavantage/gbp-usd-daily-prices-response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        client <- AlphaVantageClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.latestPrice(pair)
      yield res

      result.asserting { price =>
        price mustBe PriceRange(1.31211, 1.31472, 1.30320, 1.30395, 0, Instant.parse("2022-04-14T14:10:00Z"))
      }
    }

    "retry on client or server error" in {
      val testingBackend: SttpBackend[IO, Any] = backendStub.whenAnyRequest
        .thenRespondCyclicResponses(
          Response("uh-oh!", StatusCode.BadRequest),
          Response.ok(json("alphavantage/gbp-usd-hourly-prices-response.json"))
        )

      val result = for
        client <- AlphaVantageClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        timeSeriesData.prices must have size 100
      }
    }

    "return error when not enough data points" in {
      val testingBackend: SttpBackend[IO, Any] = backendStub.whenAnyRequest
        .thenRespond(Response.ok(json("alphavantage/gbp-usd-daily-almost-empty-response.json")))

      val result = for
        client <- AlphaVantageClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.D1)
      yield res

      result.attempt.asserting { res =>
        res mustBe Left(AppError.NotEnoughDataPoints("alpha-vantage", 2))
      }
    }
  }
}
