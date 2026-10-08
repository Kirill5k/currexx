package currexx.backtest

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import currexx.backtest.MarketDataProvider.{Dataset, DateRange}
import currexx.domain.market.{CurrencyPair, Interval, PriceRange}
import kirill5k.common.cats.test.IOWordSpec

import java.time.temporal.ChronoUnit
import java.time.{Instant, YearMonth, ZoneOffset}

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

  /** The calendar months a run of bars touched, oldest first. */
  private def monthsOf(bars: List[Instant]): List[String] = bars.map(_.toString.take(7)).distinct.sorted

  /** An export and what its first bar and length are, so that every export is held to the same expectations. */
  final private case class Export(filePath: String, firstBar: Instant, firstClose: Double, bars: Int)

  private val exports = List(
    Export("eur-usd-1h-1year-2023-07-2024-06.csv", Instant.parse("2023-07-02T21:00:00Z"), 1.09089, 6204),
    Export("eur-usd-1h-1year.csv", Instant.parse("2024-07-01T00:00:00Z"), 1.07456, 6113),
    Export("eur-usd-1h-1year-2025-07-2026-06.csv", Instant.parse("2025-07-01T00:00:00Z"), 1.17973, 6226)
  )

  private val fullFixture   = "eur-usd-1h-walk-forward-full.csv"
  private val firstFixture  = "eur-usd-1h-walk-forward-first.csv"
  private val secondFixture = "eur-usd-1h-walk-forward-second.csv"
  private val splitFixture  = Dataset(NonEmptyList.of(firstFixture, secondFixture))

  private def range(from: String, until: String): Option[DateRange] = Some(DateRange(YearMonth.parse(from), YearMonth.parse(until)))

  private def failsReading(dataset: Dataset, message: String): IO[org.scalatest.Assertion] =
    MarketDataProvider.read[IO](dataset).compile.toList.attempt.map {
      case Left(error: IllegalArgumentException) => error.getMessage must include(message)
      case other                                 => fail(s"Expected an invalid dataset, got $other")
    }

  "MarketDataProvider.read" should {

    "parse the UTC timestamps every export is stamped with" in
      // The offset is spelled +00:00 rather than Z, which Instant.parse would reject.
      exports
        .parTraverse(e => pricesOf(e.filePath).map(_.head))
        .asserting(_.map(bar => (bar.time, bar.close)) mustBe exports.map(e => (e.firstBar, e.firstClose)))

    "read every export as an hourly series for the pair named in the file" in {
      val expected = (CurrencyPair.fromUnsafe("EURUSD"), Interval.H1)

      exports
        .traverse(e => MarketDataProvider.read[IO](Dataset(e.filePath)).head.compile.lastOrError)
        .asserting(_.map(d => (d.currencyPair, d.interval)) mustBe exports.map(_ => expected))
    }

    "keep every row of every export, since none of them pads the hours the market was shut" in
      // A padded row would carry a volume of 0 and repeat the previous close as all four prices, which the volume
      // filter in `read` drops; anything it dropped here would be a row of real trading gone missing.
      exports.parTraverse(e => pricesOf(e.filePath)).asserting { series =>
        series.flatten.map(_.volume).filter(_ <= 0) mustBe empty
        series.map(_.size) mustBe exports.map(_.bars)
      }

    "return every series oldest first within each window" in
      exports
        .traverse(e => MarketDataProvider.read[IO](Dataset(e.filePath)).head.compile.lastOrError)
        .asserting { data =>
          // PriceRange lists run latest to earliest, which every calculation in the calculations module relies on.
          data.foreach { d =>
            val times = d.prices.toList.map(_.time)
            times mustBe times.sortBy(-_.toEpochMilli)
          }
          succeed
        }
  }

  "MarketDataProvider combined history" should {

    "produce identical windows when the same raw bars are split between files" in
      (for
        single <- MarketDataProvider.read[IO](Dataset(fullFixture)).compile.toList
        split  <- MarketDataProvider.read[IO](splitFixture).compile.toList
      yield {
        split mustBe single
        split must have size 121
        split.map(_.latestTime).distinct must have size 121
      }).asserting(identity)

    "warm the opening bar from the preceding file without consuming the requested period" in {
      val period = range("2024-02", "2024-03")
      (for
        single <- MarketDataProvider.read[IO](Dataset(fullFixture, period)).compile.toList
        split  <- MarketDataProvider.read[IO](splitFixture.copy(range = period)).compile.toList
      yield {
        split mustBe single
        split must have size 120
        split.head.latestTime mustBe Instant.parse("2024-02-01T00:00:00Z")
        split.head.prices.size mustBe 100
        split.head.prices.tail.forall(_.time.isBefore(split.head.latestTime)) mustBe true
        split.head.prices.last.time mustBe Instant.parse("2024-01-27T21:00:00Z")
      }).asserting(identity)
    }

    "stop before the exclusive cutoff without opening a subsequent file" in {
      val bounded = Dataset(NonEmptyList.of(fullFixture, "eur-usd-1h-file-that-does-not-exist.csv"), range("2024-01", "2024-02"))
      MarketDataProvider.read[IO](bounded).compile.toList.asserting { windows =>
        windows.map(_.latestTime) mustBe List(Instant.parse("2024-01-31T23:00:00Z"))
        windows.flatMap(_.prices.toList).forall(_.time.isBefore(Instant.parse("2024-02-01T00:00:00Z"))) mustBe true
      }
    }

    "avoid creating a short warm-up window when the date cutoff precedes the hundredth bar" in
      MarketDataProvider
        .read[IO](Dataset("eur-usd-1h-walk-forward-short-before-cutoff.csv", range("2024-01", "2024-02")))
        .compile
        .toList
        .asserting(_ mustBe empty)

    "preserve one partial window when a short export ends naturally" in
      MarketDataProvider.read[IO](Dataset("eur-usd-1h-walk-forward-missing-month.csv")).compile.toList.asserting { windows =>
        windows must have size 1
        windows.head.prices.toList.map(_.time) mustBe List(
          Instant.parse("2024-03-01T00:00:00Z"),
          Instant.parse("2024-01-01T00:00:00Z")
        )
        windows.head.prices.toList.forall(_.volume > 0) mustBe true
      }

    "emit no windows when an export contains only zero-volume rows" in
      MarketDataProvider.read[IO](Dataset("eur-usd-1h-walk-forward-empty-history.csv")).compile.toList.asserting(_ mustBe Nil)

    "reject files from different pairs or intervals before loading them" in
      List(
        Dataset(NonEmptyList.of(firstFixture, "aud-usd-1h-missing.csv"))   -> "mixes currency pairs",
        Dataset(NonEmptyList.of(firstFixture, "eur-usd-1day-missing.csv")) -> "mixes intervals"
      ).traverse { case (dataset, message) => failsReading(dataset, message) }.asserting(_ => succeed)

    "reject reversed bars and overlapping exports rather than silently sorting or deduplicating" in
      List(
        Dataset("eur-usd-1h-walk-forward-unordered.csv"),
        Dataset("eur-usd-1h-walk-forward-duplicate.csv"),
        Dataset(NonEmptyList.of("eur-usd-1h-walk-forward-empty-history.csv", "eur-usd-1h-walk-forward-empty-history.csv")),
        Dataset(NonEmptyList.of(firstFixture, fullFixture)),
        Dataset(NonEmptyList.of(secondFixture, firstFixture))
      ).traverse(failsReading(_, "strictly increasing")).asserting(_ => succeed)

    "reject a requested month containing only zero-volume bars" in {
      val dataset = Dataset("eur-usd-1h-walk-forward-missing-month.csv", range("2024-01", "2024-04"))
      failsReading(dataset, "no trading bars in requested month(s) 2024-02").asserting(identity)
    }

    "reject missing leading and trailing requested months" in
      List(
        Dataset(fullFixture, range("2023-12", "2024-03")) -> "2023-12",
        Dataset(fullFixture, range("2024-01", "2024-04")) -> "2024-03"
      ).traverse { case (dataset, month) => failsReading(dataset, s"no trading bars in requested month(s) $month") }.asserting(_ => succeed)

    "reject empty or reversed calendar ranges" in
      List(range("2024-02", "2024-02"), range("2024-03", "2024-02"))
        .traverse(period => failsReading(Dataset(fullFixture, period), "range must be nonempty and increasing"))
        .asserting(_ => succeed)

    "group all three annual exports per pair and retain history over the June boundary" in {
      val histories = MarketDataProvider.majors1hHistory
      val eur       = histories.find(_.currencyPair == CurrencyPair.fromUnsafe("EURUSD")).get
      MarketDataProvider.read[IO](eur.copy(range = range("2024-07", "2024-08"))).head.compile.lastOrError.asserting { first =>
        histories must have size 6
        histories.map(_.currencyPair).distinct must have size 6
        histories.foreach(_.filePaths.size mustBe 3)
        first.latestTime mustBe Instant.parse("2024-07-01T00:00:00Z")
        first.prices.size mustBe 100
        first.prices.last.time.isBefore(Instant.parse("2024-07-01T00:00:00Z")) mustBe true
      }
    }
  }

  "MarketDataProvider.fingerprint" should {

    "hash the original resource bytes with SHA-256" in
      MarketDataProvider.fingerprint[IO](fullFixture).asserting {
        _ mustBe "98d777fa99500280cee870b259f98fdfa0120109539f1d65c398d2b95189bf8c"
      }
  }

  "MarketDataProvider.inspectHistory" should {

    "count every raw trading bar identically across one or several exports" in
      (for
        single <- MarketDataProvider.inspectHistory[IO](Dataset(fullFixture))
        split  <- MarketDataProvider.inspectHistory[IO](splitFixture)
      yield (single, split)).asserting { case (single, split) =>
        split mustBe single
        split.currencyPair mustBe CurrencyPair.fromUnsafe("EURUSD")
        split.interval mustBe Interval.H1
        split.firstBar mustBe Instant.parse("2024-01-27T20:00:00Z")
        split.lastBar mustBe Instant.parse("2024-02-05T23:00:00Z")
        split.perMonth mustBe Map(YearMonth.of(2024, 1) -> 100L, YearMonth.of(2024, 2) -> 120L)
        split.countBefore(YearMonth.of(2024, 2)) mustBe 100L
        split.countIn(DateRange(YearMonth.of(2024, 1), YearMonth.of(2024, 3))) mustBe 220L
      }

    "expose unsupported older and future months without inventing coverage or warm-up" in
      MarketDataProvider.inspectHistory[IO](Dataset(fullFixture)).asserting { coverage =>
        coverage.perMonth.contains(YearMonth.of(2023, 12)) mustBe false
        coverage.perMonth.contains(YearMonth.of(2024, 3)) mustBe false
        coverage.countBefore(YearMonth.of(2023, 12)) mustBe 0L
        coverage.countBefore(YearMonth.of(2024, 1)) mustBe 0L
        coverage.countIn(DateRange(YearMonth.of(2023, 12), YearMonth.of(2024, 1))) mustBe 0L
        coverage.countIn(DateRange(YearMonth.of(2024, 3), YearMonth.of(2024, 4))) mustBe 0L
      }

    "exclude zero-volume rows from monthly coverage and warm-up availability" in
      MarketDataProvider.inspectHistory[IO](Dataset("eur-usd-1h-walk-forward-missing-month.csv")).asserting { coverage =>
        coverage.perMonth mustBe Map(YearMonth.of(2024, 1) -> 1L, YearMonth.of(2024, 3) -> 1L)
        coverage.countBefore(YearMonth.of(2024, 3)) mustBe 1L
        coverage.countIn(DateRange(YearMonth.of(2024, 2), YearMonth.of(2024, 3))) mustBe 0L
      }

    "validate all declared exports for strict ordering, duplicates and consistent metadata" in
      List(
        Dataset("eur-usd-1h-walk-forward-unordered.csv") -> "strictly increasing",
        Dataset("eur-usd-1h-walk-forward-duplicate.csv") -> "strictly increasing",
        Dataset(
          NonEmptyList.of("eur-usd-1h-walk-forward-empty-history.csv", "eur-usd-1h-walk-forward-empty-history.csv")
        )                                                                  -> "strictly increasing",
        Dataset(NonEmptyList.of(firstFixture, fullFixture))                -> "strictly increasing",
        Dataset(NonEmptyList.of(secondFixture, firstFixture))              -> "strictly increasing",
        Dataset(NonEmptyList.of(firstFixture, "aud-usd-1h-missing.csv"))   -> "mixes currency pairs",
        Dataset(NonEmptyList.of(firstFixture, "eur-usd-1day-missing.csv")) -> "mixes intervals"
      ).traverse { case (dataset, message) =>
        MarketDataProvider.inspectHistory[IO](dataset).attempt.map {
          case Left(error: IllegalArgumentException) => error.getMessage must include(message)
          case other                                 => fail(s"Expected invalid history, got $other")
        }
      }.asserting(_ => succeed)

    "reject histories without any trading bars" in
      MarketDataProvider.inspectHistory[IO](Dataset("eur-usd-1h-walk-forward-empty-history.csv")).attempt.asserting {
        case Left(error: IllegalArgumentException) => error.getMessage must include("contains no trading bars")
        case other                                 => fail(s"Expected unavailable history, got $other")
      }

  }

  "MarketDataProvider search folds" should {

    "carve the corpus into segments that share no bar" in
      // A single bar in two folds is one stretch of market counted twice in the aggregate, and a candidate fitted to it
      // rewarded twice. A single bar shared with the validation segment is a bar of the champion's evidence that the
      // search was allowed to fit, and there is nothing downstream that would notice either.
      (MarketDataProvider.majors1hSearchFolds.map(_.head) :+ MarketDataProvider.majors1hValidationFold.head)
        .parTraverse(tradedBars)
        .asserting { segments =>
          segments.combinations(2).foreach(pair => pair.head.toSet.intersect(pair.last.toSet) mustBe empty)
          segments.sliding(2).foreach {
            case List(earlier, later) => earlier.max.isBefore(later.min) mustBe true
            case _                    => succeed
          }
          succeed
        }

    "give every segment the calendar months it is scored over" in
      // Every consistency threshold is counted in months, so each of them is measured on less as a segment shortens.
      // Four, which divides a twelve-month export into exactly three folds and gives the validation segment the same
      // length, so the thresholds mean the same thing on both. Six folds, because the search spans two exports: three
      // months of 2023-07..2024-06 followed by three of 2024-07..2025-07, contiguous across the boundary between them.
      (MarketDataProvider.majors1hSearchFolds.map(_.head) :+ MarketDataProvider.majors1hValidationFold.head)
        .parTraverse(dataset => tradedBars(dataset).map(monthsOf))
        .asserting { segments =>
          segments mustBe List(
            List("2023-07", "2023-08", "2023-09", "2023-10"),
            List("2023-11", "2023-12", "2024-01", "2024-02"),
            List("2024-03", "2024-04", "2024-05", "2024-06"),
            List("2024-07", "2024-08", "2024-09", "2024-10"),
            List("2024-11", "2024-12", "2025-01", "2025-02"),
            List("2025-03", "2025-04", "2025-05", "2025-06"),
            List("2025-08", "2025-09", "2025-10", "2025-11")
          )
        }

    "hold back a segment that the search never reads" in
      // The folds are scored against and the validation segment ranks the shortlist, so both are spent by the time a
      // round ends. This is what is left to say whether the champion generalises, and it only says it once.
      tradedBars(MarketDataProvider.majors1hHoldout.head).map(monthsOf).asserting {
        _ mustBe List("2025-12", "2026-01", "2026-02", "2026-03", "2026-04", "2026-05", "2026-06")
      }

    "open every segment that has history behind it on its own first month" in {
      // The defect this exists to catch is silent. `read` windows the whole file and keeps the windows whose newest bar
      // lands in the segment, so a segment starting mid-file inherits its 99 bars of history free and is offered every
      // bar it owns. Lose that and it would spend its own first hundred bars becoming a window instead, and the month
      // labels above could not tell: the truncated month is still the month it always was, and `coveredMonths` bills it
      // in full either way. Every scored segment starts mid-file except the first fold of each export, which has nothing
      // before it to inherit; those two are asserted below.
      val opensItsOwnExport = Set(0, 3)
      val scored            = MarketDataProvider.majors1hSearchFolds.zipWithIndex.collect {
        case (fold, index) if !opensItsOwnExport(index) => fold.head
      } ::: List(MarketDataProvider.majors1hValidationFold.head, MarketDataProvider.majors1hHoldout.head)

      scored
        .parTraverse(dataset => MarketDataProvider.read[IO](dataset).head.compile.lastOrError.map(dataset -> _))
        .asserting { opened =>
          opened.foreach { case (dataset, first) =>
            val segmentStart = dataset.range.get.from.atDay(1).atStartOfDay(ZoneOffset.UTC).toInstant
            withClue(s"$dataset: ") {
              // A full window, drawn from before the segment rather than out of it.
              first.prices.size mustBe 100
              first.prices.last.time.isBefore(segmentStart) mustBe true
              // And the first bar actually offered is the segment's own opening bar, not one a week into it. Three days
              // of slack because a month can open on a weekend, when the market is shut and the export has no rows.
              first.latestTime.isBefore(segmentStart.plus(3, ChronoUnit.DAYS)) mustBe true
            }
          }
          succeed
        }
    }

    "spend each export's opening fold's first hundred bars on its first window rather than a month of the export" in
      // The accepted cost of scoring all twelve months of an export: its opening fold starts on the file's first bar, so
      // it has no history to inherit and its first hundred bars — five days, which `coveredMonths` still bills as a
      // whole month — never reach a strategy. The alternative is handing a whole month of a twelve-month export to
      // warm-up for the sake of the 99 bars `read` actually needs, which costs a quarter of a fold to save five days.
      //
      // Paid twice now rather than once, the search folds spanning two exports: folds 1 and 4 each open their own file.
      // Still tolerable, and for the same two reasons: it understates rather than flatters, and it lands on search
      // folds. On the segment that ranks the shortlist it would bias the one measurement the whole split exists to keep
      // clean, which is why the validation fold still opens a month into its own file.
      List(
        MarketDataProvider.majors1hSearchFolds.head -> ("2023-07-02T21:00:00Z", "2023-07-07T00:00:00Z"),
        MarketDataProvider.majors1hSearchFolds(3)   -> ("2024-07-01T00:00:00Z", "2024-07-05T03:00:00Z")
      ).parTraverse { case (fold, (oldest, latest)) =>
        fold
          .parTraverse(dataset => MarketDataProvider.read[IO](dataset).head.compile.lastOrError.map(dataset -> _))
          .map(opened => (opened, oldest, latest))
      }.asserting { folds =>
        folds.foreach { case (opened, oldest, latest) =>
          opened.foreach { case (dataset, first) =>
            withClue(s"$dataset: ") {
              first.prices.size mustBe 100
              // Every bar of the window comes out of the fold itself, there being nothing before it.
              first.prices.last.time mustBe Instant.parse(oldest)
              first.latestTime mustBe Instant.parse(latest)
            }
          }
        }
        succeed
      }
  }
}
