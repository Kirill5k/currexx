package currexx.clients.data.alphavantage

import cats.effect.{IO, Ref}
import currexx.domain.errors.AppError
import currexx.domain.market.{CandleCount, CurrencyPair, Interval, PriceRange}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import currexx.domain.market.Currency.{GBP, USD}
import kirill5k.common.sttp.test.Sttp4WordSpec
import io.circe.{Json, JsonObject}
import sttp.client4.testing.ResponseStub
import sttp.model.StatusCode

import java.time.{Instant, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.*

class AlphaVantageClientSpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

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
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("alpha-vantage.com/query") && r.hasParams(params) =>
            ResponseStub.adjust(readJson("alphavantage/gbp-usd-hourly-prices-response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        client <- AlphaVantageClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.H1, CandleCount.Default)
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
      val params         = Map("function" -> "FX_DAILY", "from_symbol" -> "GBP", "to_symbol" -> "USD", "apikey" -> "api-key")
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("alpha-vantage.com/query") && r.hasParams(params) =>
            ResponseStub.adjust(readJson("alphavantage/gbp-usd-daily-prices-response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        client <- AlphaVantageClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.D1, CandleCount.Default)
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
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("alpha-vantage.com/query") =>
            ResponseStub.adjust(readJson("alphavantage/gbp-usd-daily-prices-response.json"))
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

    List(
      (Interval.H1, "FX_INTRADAY", "Time Series FX (60min)", "alphavantage/gbp-usd-hourly-prices-response.json"),
      (Interval.D1, "FX_DAILY", "Time Series FX (Daily)", "alphavantage/gbp-usd-daily-prices-response.json")
    ).foreach { case (interval, function, field, resource) =>
      s"select compact or full $interval history and trim each cached response to the requested count" in {
        val response = io.circe.parser.parse(readJson(resource)).toOption.get
        val price    = response.hcursor.downField(field).focus.get.asObject.get.values.head
        val start    = LocalDateTime.of(2022, 4, 14, 0, 0)
        val prices   = JsonObject.fromIterable((0 until 150).map { i =>
          val time =
            if (interval == Interval.D1) start.minusDays(i).toLocalDate.toString
            else start.minusHours(i).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
          time -> price
        })
        val fullResponse = response.hcursor.downField(field).withFocus(_ => Json.fromJsonObject(prices)).top.get.noSpaces

        val result = for
          calls <- Ref.of[IO, Int](0)
          backend = fs2BackendStub
            .whenRequestMatches(_.hasParams(Map("function" -> function, "outputsize" -> "compact")))
            .thenRespondF(calls.update(_ + 1).as(ResponseStub.adjust(response.noSpaces)))
            .whenRequestMatches(_.hasParams(Map("function" -> function, "outputsize" -> "full")))
            .thenRespondF(calls.update(_ + 1).as(ResponseStub.adjust(fullResponse)))
          client <- AlphaVantageClient.make[IO](config, backend, 100.millis)
          small  <- client.timeSeriesData(pair, interval, CandleCount.from(80).toOption.get)
          normal <- client.timeSeriesData(pair, interval, CandleCount.Default)
          large  <- client.timeSeriesData(pair, interval, CandleCount.from(125).toOption.get)
          larger <- client.timeSeriesData(pair, interval, CandleCount.from(140).toOption.get)
          count  <- calls.get
        yield (small, normal, large, larger, count)

        result.asserting { case (small, normal, large, larger, count) =>
          small.prices must have size 80
          normal.prices must have size 100
          large.prices must have size 125
          larger.prices must have size 140
          small.prices.toList mustBe normal.prices.take(80)
          large.prices.toList mustBe larger.prices.take(125)
          count mustBe 2
        }
      }
    }

    "retry on client or server error" in {
      val testingBackend = fs2BackendStub.whenAnyRequest
        .thenRespondCyclic(
          ResponseStub.adjust("uh-oh!", StatusCode.BadRequest),
          ResponseStub.adjust(readJson("alphavantage/gbp-usd-hourly-prices-response.json"))
        )

      val result = for
        client <- AlphaVantageClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.H1, CandleCount.Default)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        timeSeriesData.prices must have size 100
      }
    }

    "return error when not enough data points" in {
      val testingBackend = fs2BackendStub.whenAnyRequest
        .thenRespond(ResponseStub.adjust(readJson("alphavantage/gbp-usd-daily-almost-empty-response.json")))

      val result = for
        client <- AlphaVantageClient.make[IO](config, testingBackend, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.D1, CandleCount.Default)
      yield res

      result.assertThrows(AppError.NotEnoughDataPoints("alpha-vantage", 2))
    }
  }
}
