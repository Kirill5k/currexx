package currexx.clients.data.oanda

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

class OandaDataClientSpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val config = OandaDataConfig("http://oanda.com", "api-key")
  val pair   = CurrencyPair(EUR, USD)

  "An OandaDataClient" should {
    "retrieve the latest 100 completed candles in reverse chronological order" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-02-06T22:00:00Z"))

      val requestParams = Map(
        "granularity"       -> "H1",
        "count"             -> "150",
        "alignmentTimezone" -> "UTC"
      )

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("oanda.com/v3/instruments/EUR_USD/candles") && r.hasParams(requestParams) =>
            ResponseStub.adjust(readJson("oanda/instruments-rud-usd-success-response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        cache <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client = LiveOandaDataClient[IO](testingBackend, config, cache)
        res <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        timeSeriesData.prices must have size 100
        // Should be reversed: latest (2026-02-06T21:00:00Z) first
        timeSeriesData.prices.head mustBe PriceRange(1.18222, 1.18241, 1.18134, 1.18168, 1859.0, Instant.parse("2026-02-06T21:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.18002, 1.18013, 1.17777, 1.17796, 9007.0, Instant.parse("2026-02-02T18:00:00Z"))
        val times = timeSeriesData.prices.toList.map(_.time)
        times mustBe times.sortWith(_.isAfter(_))
      }
    }

    "exclude the incomplete current candle before keeping the latest 100 candles" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-02-06T21:03:00Z"))

      val requestParams = Map(
        "granularity"       -> "H1",
        "count"             -> "150",
        "alignmentTimezone" -> "UTC"
      )

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("oanda.com/v3/instruments/EUR_USD/candles") && r.hasParams(requestParams) =>
            ResponseStub.adjust(readJson("oanda/instruments-rud-usd-success-response.json"))
          case _ => throw new RuntimeException()
        }

      val result = for
        cache <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client = LiveOandaDataClient[IO](testingBackend, config, cache)
        res <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        timeSeriesData.prices must have size 100
        timeSeriesData.prices.head mustBe PriceRange(1.18195, 1.18229, 1.18184, 1.18222, 2336.0, Instant.parse("2026-02-06T20:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.18066, 1.18125, 1.17949, 1.18000, 10070.0, Instant.parse("2026-02-02T17:00:00Z"))
        val times = timeSeriesData.prices.toList.map(_.time)
        times mustBe times.sortWith(_.isAfter(_))
      }
    }

    "use configured fetch and signal candle counts" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-02-06T21:03:00Z"))

      val customConfig  = config.copy(fetchCandleCount = 120, signalCandleCount = 80)
      val requestParams = Map(
        "granularity"       -> "H1",
        "count"             -> "120",
        "alignmentTimezone" -> "UTC"
      )
      val response = io.circe.parser
        .parse(readJson("oanda/instruments-rud-usd-success-response.json"))
        .toOption
        .get
        .hcursor
        .downField("candles")
        .withFocus(_.mapArray(_.takeRight(120)))
        .top
        .get
        .noSpaces

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("oanda.com/v3/instruments/EUR_USD/candles") && r.hasParams(requestParams) =>
            ResponseStub.adjust(response)
          case _ => throw new RuntimeException()
        }

      val result = for
        cache <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client = LiveOandaDataClient[IO](testingBackend, customConfig, cache)
        res <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.prices must have size 80
        timeSeriesData.prices.head mustBe PriceRange(1.18195, 1.18229, 1.18184, 1.18222, 2336.0, Instant.parse("2026-02-06T20:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.17966, 1.18068, 1.17926, 1.17961, 8804.0, Instant.parse("2026-02-03T13:00:00Z"))
      }
    }

    "reject invalid configured candle counts" in {
      an[IllegalArgumentException] must be thrownBy config.copy(signalCandleCount = 0)
      an[IllegalArgumentException] must be thrownBy config.copy(fetchCandleCount = 100)
      an[IllegalArgumentException] must be thrownBy config.copy(fetchCandleCount = 99)
    }

    "retrieve the current quote without filtering an incomplete candle" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-02-06T21:00:30Z"))

      val requestParams = Map(
        "granularity"       -> "M1",
        "count"             -> "1",
        "alignmentTimezone" -> "UTC"
      )
      val response = """{
        "instrument": "EUR_USD",
        "granularity": "M1",
        "candles": [{
          "complete": false,
          "volume": 10,
          "time": "2026-02-06T21:00:00Z",
          "mid": {"o": "1.18222", "h": "1.18241", "l": "1.18134", "c": "1.18168"}
        }]
      }"""

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("oanda.com/v3/instruments/EUR_USD/candles") && r.hasParams(requestParams) =>
            ResponseStub.adjust(response)
          case _ => throw new RuntimeException()
        }

      val result = for
        cache <- Cache.make[IO, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
        client = LiveOandaDataClient[IO](testingBackend, config, cache)
        res <- client.latestPrice(pair)
      yield res

      result.asserting { price =>
        price mustBe PriceRange(1.18222, 1.18241, 1.18134, 1.18168, 10.0, Instant.parse("2026-02-06T21:00:00Z"))
      }
    }
  }
}
