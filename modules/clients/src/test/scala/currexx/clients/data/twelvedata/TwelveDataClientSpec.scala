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
        "timezone"   -> "UTC"
      )

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
            ResponseStub.adjust(readJson("twelvedata/eur-usd-daily-prices.response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        cache  <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client <- TwelveDataClient.make[IO](config, testingBackend, cache, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.D1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.D1
        timeSeriesData.prices must have size 100
        timeSeriesData.prices.head mustBe PriceRange(0.99105, 1.0034, 0.9899, 1.0025, 0.0, Instant.parse(s"2022-11-07T00:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.0424, 1.0463, 1.0418, 1.0452, 0.0, Instant.parse("2022-07-03T00:00:00Z"))
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
        cache  <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client <- TwelveDataClient.make[IO](config, testingBackend, cache, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.D1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.D1
        timeSeriesData.prices must have size 100
        timeSeriesData.prices.head mustBe PriceRange(0.99105, 1.0034, 0.9899, 1.0025, 0.0, Instant.parse(s"2022-11-07T00:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.0424, 1.0463, 1.0418, 1.0452, 0.0, Instant.parse("2022-07-03T00:00:00Z"))
      }
    }

    "exclude the incomplete current candle before keeping the latest 100 candles" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T12:03:00Z"))

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
        cache  <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client <- TwelveDataClient.make[IO](config, testingBackend, cache, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        timeSeriesData.prices must have size 100
        // First candle should now be 11:00 (the 12:00 candle was excluded)
        timeSeriesData.prices.head mustBe PriceRange(1.16246, 1.16294, 1.16209, 1.16285, 0.0, Instant.parse("2026-01-19T11:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.16296, 1.16355, 1.16266, 1.16316, 0.0, Instant.parse("2026-01-15T08:00:00Z"))
        val times = timeSeriesData.prices.toList.map(_.time)
        times mustBe times.sortWith(_.isAfter(_))
      }
    }

    "retrieve the latest 100 completed candles in reverse chronological order" in {
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
        cache  <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client <- TwelveDataClient.make[IO](config, testingBackend, cache, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        timeSeriesData.prices must have size 100
        // First candle should be 12:00
        timeSeriesData.prices.head mustBe PriceRange(1.16284, 1.16284, 1.16269, 1.16272, 0.0, Instant.parse("2026-01-19T12:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.16316, 1.16399, 1.16311, 1.16362, 0.0, Instant.parse("2026-01-15T09:00:00Z"))
        val times = timeSeriesData.prices.toList.map(_.time)
        times mustBe times.sortWith(_.isAfter(_))
      }
    }

    "use configured fetch and signal candle counts" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T12:03:00Z"))

      val customConfig  = config.copy(fetchCandleCount = 120, signalCandleCount = 80)
      val requestParams = Map(
        "symbol"     -> "EUR/USD",
        "interval"   -> "1h",
        "apikey"     -> "api-key",
        "outputsize" -> "120",
        "timezone"   -> "UTC"
      )
      val response = io.circe.parser
        .parse(readJson("twelvedata/eur-usd-hourly-prices.response.json"))
        .toOption
        .get
        .hcursor
        .downField("values")
        .withFocus(_.mapArray(_.take(120)))
        .top
        .get
        .noSpaces

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
            ResponseStub.adjust(response)
          case _ => throw new RuntimeException()
        }

      val result = for
        cache  <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client <- TwelveDataClient.make[IO](customConfig, testingBackend, cache, 100.millis)
        res    <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.prices must have size 80
        timeSeriesData.prices.head mustBe PriceRange(1.16246, 1.16294, 1.16209, 1.16285, 0.0, Instant.parse("2026-01-19T11:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.1612, 1.16142, 1.16101, 1.16113, 0.0, Instant.parse("2026-01-16T04:00:00Z"))
      }
    }

    "reject invalid configured candle counts" in {
      an[IllegalArgumentException] must be thrownBy config.copy(signalCandleCount = 0)
      an[IllegalArgumentException] must be thrownBy config.copy(fetchCandleCount = 100)
      an[IllegalArgumentException] must be thrownBy config.copy(fetchCandleCount = 99)
    }

    "retrieve the current quote without filtering an incomplete candle" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T12:00:30Z"))

      val requestParams = Map(
        "symbol"     -> "EUR/USD",
        "interval"   -> "1min",
        "apikey"     -> "api-key",
        "outputsize" -> "1",
        "timezone"   -> "UTC"
      )
      val response = """{
        "meta": {"symbol": "EUR/USD", "interval": "1min"},
        "values": [{
          "datetime": "2026-01-19 12:00:00",
          "open": "1.16284",
          "high": "1.16284",
          "low": "1.16269",
          "close": "1.16272"
        }]
      }"""

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
            ResponseStub.adjust(response)
          case _ => throw new RuntimeException()
        }

      val result = for
        cache  <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client <- TwelveDataClient.make[IO](config, testingBackend, cache, 100.millis)
        res    <- client.latestPrice(pair)
      yield res

      result.asserting { price =>
        price mustBe PriceRange(1.16284, 1.16284, 1.16269, 1.16272, 0.0, Instant.parse("2026-01-19T12:00:00Z"))
      }
    }
  }
}
