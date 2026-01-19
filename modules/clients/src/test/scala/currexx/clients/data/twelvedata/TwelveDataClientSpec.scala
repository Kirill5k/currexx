package currexx.clients.data.twelvedata

import cats.effect.{IO, Temporal}
import currexx.domain.market.Currency.{EUR, USD}
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import kirill5k.common.sttp.test.Sttp4WordSpec
import kirill5k.common.cats.{Cache, Clock}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.testing.ResponseStub

import java.time.Instant
import scala.concurrent.duration.*

class TwelveDataClientSpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val config = TwelveDataConfig("http://twelve-data.com", "api-key")
  val pair   = CurrencyPair(EUR, USD)

  "A TwelveDataClient" should {
    "retrieve time-series data" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2022-11-08T10:15:30Z"))

      val requestParams = Map(
        "symbol"     -> "EUR/USD",
        "interval"   -> "1day",
        "apikey"     -> "api-key",
        "outputsize" -> "150",
        "timezone" -> "UTC"
      )

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
            ResponseStub.adjust(readJson("twelvedata/eur-usd-daily-prices.response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        cache <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client <- TwelveDataClient.make[IO](config, testingBackend, cache, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.D1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.D1
        timeSeriesData.prices must have size 149
        timeSeriesData.prices.head mustBe PriceRange(0.99105, 1.0034, 0.9899, 1.0025, 0.0, Instant.parse(s"2022-11-07T00:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.05475, 1.055, 1.05405, 1.0547, 0.0, Instant.parse("2022-05-07T00:00:00Z"))
      }
    }

    "retry after some delay in case of api limit error" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2022-11-08T10:15:30Z"))

      val testingBackend = fs2BackendStub.whenAnyRequest
        .thenRespondCyclic(
          ResponseStub.adjust(readJson("twelvedata/limit-error.json")),
          ResponseStub.adjust(readJson("twelvedata/eur-usd-daily-prices.response.json"))
        )

      val result = for
        cache <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client <- TwelveDataClient.make[IO](config, testingBackend, cache, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.D1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.D1
        timeSeriesData.prices must have size 149
        timeSeriesData.prices.head mustBe PriceRange(0.99105, 1.0034, 0.9899, 1.0025, 0.0, Instant.parse(s"2022-11-07T00:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.05475, 1.055, 1.05405, 1.0547, 0.0, Instant.parse("2022-05-07T00:00:00Z"))
      }
    }

    "retrieve hourly time-series data and exclude incomplete first candle" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T12:30:00Z"))

      val requestParams = Map(
        "symbol"     -> "EUR/USD",
        "interval"   -> "1h",
        "apikey"     -> "api-key",
        "outputsize" -> "150",
        "timezone"   -> "UTC"
      )

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
            ResponseStub.adjust(readJson("twelvedata/eur-usd-hourly-prices.response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        cache <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client <- TwelveDataClient.make[IO](config, testingBackend, cache, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        // First incomplete candle should be excluded, so 149 instead of 150
        timeSeriesData.prices must have size 149
        // First candle should now be 11:00 (the 12:00 candle was excluded)
        timeSeriesData.prices.head mustBe PriceRange(1.16246, 1.16294, 1.16209, 1.16285, 0.0, Instant.parse("2026-01-19T11:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.16586, 1.16656, 1.16561, 1.16645, 0.0, Instant.parse("2026-01-13T07:00:00Z"))
      }
    }

    "retrieve hourly time-series data and keep all candles when first is complete" in {
      given clock: Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T13:00:00Z"))

      val requestParams = Map(
        "symbol"     -> "EUR/USD",
        "interval"   -> "1h",
        "apikey"     -> "api-key",
        "outputsize" -> "150",
        "timezone"   -> "UTC"
      )

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
            ResponseStub.adjust(readJson("twelvedata/eur-usd-hourly-prices.response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        cache <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client <- TwelveDataClient.make[IO](config, testingBackend, cache, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        // All 150 candles should be present (12:00 candle is complete)
        timeSeriesData.prices must have size 150
        // First candle should be 12:00
        timeSeriesData.prices.head mustBe PriceRange(1.16284, 1.16284, 1.16269, 1.16272, 0.0, Instant.parse("2026-01-19T12:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.16586, 1.16656, 1.16561, 1.16645, 0.0, Instant.parse("2026-01-13T07:00:00Z"))
      }
    }
  }
}
