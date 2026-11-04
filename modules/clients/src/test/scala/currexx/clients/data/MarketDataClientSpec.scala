package currexx.clients.data

import cats.effect.IO
import currexx.clients.data.oanda.{OandaDataClient, OandaDataConfig}
import currexx.clients.data.twelvedata.{TwelveDataClient, TwelveDataConfig}
import currexx.domain.market.{CandleCount, CurrencyPair, Interval}
import currexx.domain.market.Currency.{EUR, USD}
import kirill5k.common.cats.Clock
import kirill5k.common.sttp.test.Sttp4WordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.testing.ResponseStub
import sttp.model.StatusCode

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

class MarketDataClientSpec extends Sttp4WordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]
  given Clock[IO]  = Clock.mock[IO](Instant.parse("2026-01-19T13:00:00Z"))

  "A MarketDataClient" should {
    "preserve the requested candle count when falling back to TwelveData" in {
      val primaryCalls  = AtomicInteger(0)
      val fallbackCalls = AtomicInteger(0)
      val backend       = fs2BackendStub.whenRequestMatchesPartial {
        case r if r.isGoingTo("oanda.com/v3/instruments/EUR_USD/candles") && r.hasParams(Map("count" -> "126")) =>
          primaryCalls.incrementAndGet()
          ResponseStub.adjust("Forbidden", StatusCode.Forbidden)
        case r if r.isGoingTo("twelve-data.com/time_series") && r.hasParams(Map("outputsize" -> "126")) =>
          fallbackCalls.incrementAndGet()
          ResponseStub.adjust(readJson("twelvedata/eur-usd-hourly-prices.response.json"))
        case _ => throw new IllegalArgumentException("Unexpected market data request")
      }

      val result = for
        oanda      <- OandaDataClient.make[IO](OandaDataConfig("http://oanda.com", "api-key"), backend)
        twelveData <- TwelveDataClient.make[IO](TwelveDataConfig("http://twelve-data.com", "api-key"), backend, 100.millis)
        client     <- MarketDataClient.make[IO](twelveData, oanda)
        data       <- client.timeSeriesData(CurrencyPair(EUR, USD), Interval.H1, CandleCount.from(125).toOption.get)
      yield data

      result.asserting { data =>
        data.prices must have size 125
        data.source mustBe "twelve-data"
        primaryCalls.get mustBe 1
        fallbackCalls.get mustBe 1
      }
    }
  }
}
