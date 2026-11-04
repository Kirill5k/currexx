package currexx.backtest.services

import cats.data.NonEmptyList
import cats.effect.{IO, Ref}
import currexx.domain.market.{CandleCount, CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import kirill5k.common.cats.test.IOWordSpec

import java.time.Instant

class TestClientsSpec extends IOWordSpec {
  "TestMarketDataClient" should {
    "return the requested latest candles and preserve shorter available history" in {
      val prices = NonEmptyList.fromListUnsafe((1 to 4).toList.map { offset =>
        PriceRange(1, 1, 1, 1, 1, Instant.EPOCH.minusSeconds(offset.toLong))
      })
      val data = MarketTimeSeriesData(CurrencyPair.fromUnsafe("EURUSD"), Interval.H1, prices, "test")
      (for
        ref <- Ref.of[IO, Option[MarketTimeSeriesData]](Some(data))
        client = new TestMarketDataClient[IO](ref)
        limited <- client.timeSeriesData(data.currencyPair, data.interval, CandleCount.from(2).toOption.get)
        short <- client.timeSeriesData(data.currencyPair, data.interval, CandleCount.Default)
        latest <- client.latestPrice(data.currencyPair)
      yield {
        limited.prices.toList mustBe prices.toList.take(2)
        limited.currencyPair mustBe data.currencyPair
        limited.interval mustBe data.interval
        short mustBe data
        latest mustBe prices.head
      }).asserting(identity)
    }
  }
}
