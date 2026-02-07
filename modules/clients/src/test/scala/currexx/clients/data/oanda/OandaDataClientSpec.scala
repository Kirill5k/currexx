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
    "retrieve time-series data and reverse it" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-02-06T22:00:00Z"))

      val requestParams = Map(
        "granularity" -> "H1",
        "count"       -> "150",
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
        res    <- client.timeSeriesData(pair, Interval.H1)
      yield res

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.H1
        timeSeriesData.prices must have size 150
        // Should be reversed: latest (2026-02-06T21:00:00Z) first
        timeSeriesData.prices.head mustBe PriceRange(1.18222, 1.18241, 1.18134, 1.18168, 1859.0, Instant.parse("2026-02-06T21:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.19294, 1.19530, 1.19272, 1.19426, 18962.0, Instant.parse("2026-01-29T16:00:00Z"))
      }
    }
  }
}
