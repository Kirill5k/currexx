package currexx.backtest

import cats.effect.IO
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import currexx.backtest.MarketDataProvider.Dataset
import currexx.domain.market.{CurrencyPair, Interval, PriceRange}
import kirill5k.common.cats.test.IOWordSpec

import java.time.Instant

class MarketDataProviderSpec extends IOWordSpec {

  private def pricesOf(filePath: String): IO[List[PriceRange]] =
    MarketDataProvider
      .read[IO](Dataset(filePath))
      .compile
      .toList
      // Consecutive sliding windows overlap, so the first window holds the oldest 100 bars and every window after it
      // contributes exactly one newer bar. Reading only the head of each restores the series in chronological order.
      .map(data => data.head.prices.toList.reverse ++ data.tail.map(_.prices.head))

  /** The bars a segment actually offers a strategy, which is the newest bar of each window it emits. */
  private def tradedBars(dataset: Dataset): IO[List[Instant]] =
    MarketDataProvider.read[IO](dataset).compile.toList.map(_.map(_.latestTime))

  private val legacyFile = "eur-usd-1h-1year.csv"
  private val utcFile    = "eur-usd-1h-1year-2025-07-2026-06.csv"

  "MarketDataProvider.read" should {

    "parse the legacy export's local timestamps into UTC" in
      // The first row the volume filter keeps is 07.07.2024 22:00:00.000 GMT+0100, which is 21:00 UTC. Reading the
      // offset as UTC instead of applying it would shift the whole series an hour and misplace every session boundary.
      pricesOf(legacyFile).asserting { prices =>
        prices.head.time mustBe Instant.parse("2024-07-07T21:00:00Z")
        prices.head.close mustBe 1.08177
      }

    "parse the newer export's UTC timestamps" in
      // The offset is spelled +00:00 rather than Z, which Instant.parse would reject.
      pricesOf(utcFile).asserting { prices =>
        prices.head.time mustBe Instant.parse("2025-07-01T00:00:00Z")
        prices.head.close mustBe 1.17973
      }

    "read both exports as hourly series for the pair named in the file" in {
      val expected = (CurrencyPair.fromUnsafe("EURUSD"), Interval.H1)

      List(legacyFile, utcFile)
        .traverse(f => MarketDataProvider.read[IO](Dataset(f)).head.compile.lastOrError)
        .asserting(_.map(d => (d.currencyPair, d.interval)) mustBe List(expected, expected))
    }

    "put the volumes of both exports on the same scale" in {
      // The newer files count whole base-currency units and the legacy ones count millions, so without rescaling these
      // averages would sit six orders of magnitude apart. Nothing downstream is scale-sensitive today — CMF is a ratio
      // of volumes — but a series whose units change halfway through the corpus is a trap for whatever is added next.
      val averageVolume = (prices: List[PriceRange]) => prices.map(_.volume).sum / prices.size

      (pricesOf(legacyFile), pricesOf(utcFile)).parTupled.asserting { case (legacy, utc) =>
        val ratio = averageVolume(legacy) / averageVolume(utc)
        ratio must be > 0.1
        ratio must be < 10.0
      }
    }

    "drop the rows the legacy export pads closed market hours with" in
      // Those rows carry a volume of 0 and repeat the previous close as all four prices, which would otherwise show up
      // as thousands of hours of perfectly flat price action that never happened.
      pricesOf(legacyFile).asserting { prices =>
        prices.map(_.volume).filter(_ <= 0) mustBe empty
        prices.size mustBe 6322
      }

    "keep every row of the newer export, which omits closed hours already" in
      pricesOf(utcFile).asserting(_.size mustBe 6226)

    "return both series oldest first within each window" in
      List(legacyFile, utcFile)
        .traverse(f => MarketDataProvider.read[IO](Dataset(f)).head.compile.lastOrError)
        .asserting { data =>
          // PriceRange lists run latest to earliest, which every calculation in the calculations module relies on.
          data.foreach { d =>
            val times = d.prices.toList.map(_.time)
            times mustBe times.sortBy(-_.toEpochMilli)
          }
          succeed
        }
  }

  "MarketDataProvider training and validation segments" should {

    "split the older export into two halves that share no bar" in
      // A single bar in both halves is a single bar of the champion's evidence that the search was allowed to fit, and
      // there is nothing downstream that would notice.
      (tradedBars(MarketDataProvider.majors1hTraining.head), tradedBars(MarketDataProvider.majors1hValidation.head)).parTupled
        .asserting { case (training, validation) =>
          training.toSet.intersect(validation.toSet) mustBe empty
          training.max.isBefore(validation.min) mustBe true
        }

    "give each half the six calendar months it is scored over" in {
      // Six because the consistency thresholds a candidate is scored against are counted in months, and a validation
      // half shorter than `minMonthsCovered` would report a breach against every candidate alike.
      val monthsOf = (bars: List[Instant]) => bars.map(_.toString.take(7)).distinct.sorted

      (tradedBars(MarketDataProvider.majors1hTraining.head), tradedBars(MarketDataProvider.majors1hValidation.head)).parTupled
        .asserting { case (training, validation) =>
          monthsOf(training) mustBe List("2024-07", "2024-08", "2024-09", "2024-10", "2024-11", "2024-12")
          monthsOf(validation) mustBe List("2025-01", "2025-02", "2025-03", "2025-04", "2025-05", "2025-06")
        }
    }

    "carry the bars preceding a segment into it as history rather than spending its own on warm-up" in {
      // The validation half opens on the first bar of January with a hundred bars of December already behind it, so a
      // candidate is judged on every month it was given. Filtering rows before the windows were formed would instead
      // start it a hundred bars into January, and the same bar would produce different indicator values depending on
      // which segment it had been read into.
      val dataset = MarketDataProvider.majors1hValidation.head

      MarketDataProvider.read[IO](dataset).head.compile.lastOrError.asserting { first =>
        first.latestTime.toString must startWith("2025-01-01")
        first.prices.size mustBe 100
        first.prices.last.time.isBefore(Instant.parse("2025-01-01T00:00:00Z")) mustBe true
      }
    }
  }
}
