package currexx.clients.data

import cats.data.{NonEmptyList, WriterT}
import cats.effect.IO
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.extras.{LogLevel, LogMessage, WriterTLogger}

import java.time.Instant

class SignalCandlesSpec extends IOWordSpec {
  private type Logged[A] = WriterT[IO, List[LogMessage], A]
  private given Logger[Logged] = WriterTLogger[IO, List]()

  private val now = Instant.parse("2026-01-19T12:03:00Z")

  private def candle(time: String): PriceRange = PriceRange(1, 1, 1, 1, 0, Instant.parse(time))

  "Signal candle preparation" should {
    "retain a sole incomplete candle and emit the existing warning" in {
      val data = MarketTimeSeriesData(
        CurrencyPair.fromUnsafe("EURUSD"),
        Interval.H1,
        NonEmptyList.one(candle("2026-01-19T12:00:00Z")),
        "test"
      )
      val warning = "Only incomplete candle available for EURUSD at H1"
      SignalCandles.prepare[Logged](data, Interval.H1, now, 100).run.asserting { case (logs, result) =>
        logs mustBe List(LogMessage(LogLevel.Warn, None, warning))
        result mustBe data
      }
    }

    "remove only the first incomplete candle before truncating the signal window" in {
      val data = MarketTimeSeriesData(
        CurrencyPair.fromUnsafe("EURUSD"),
        Interval.H1,
        NonEmptyList.of(
          candle("2026-01-19T13:00:00Z"),
          candle("2026-01-19T12:00:00Z"),
          candle("2026-01-19T11:00:00Z")
        ),
        "test"
      )

      SignalCandles.prepare[Logged](data, Interval.H1, now, 1).run.asserting { case (logs, result) =>
        logs mustBe empty
        result mustBe data.copy(prices = NonEmptyList.one(data.prices.tail.head))
      }
    }
  }
}
