package currexx.backtest

import cats.effect.Async
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import fs2.io.readClassResource
import fs2.{Stream, text}

import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, YearMonth, ZoneOffset, ZonedDateTime}

object MarketDataProvider:

  /** A half-open span of calendar months, `[from, until)`, used to carve one export into disjoint segments.
    *
    * Months rather than instants because everything downstream counts calendar months: a mid-month boundary would hand the segments either
    * side of it a stub month, scored as though it had been offered a full month's trading.
    */
  final case class DateRange(from: YearMonth, until: YearMonth) {
    private val fromTime  = from.atDay(1).atStartOfDay(ZoneOffset.UTC).toInstant
    private val untilTime = until.atDay(1).atStartOfDay(ZoneOffset.UTC).toInstant

    def contains(time: Instant): Boolean = !time.isBefore(fromTime) && time.isBefore(untilTime)
    override def toString: String        = s"$from..${until.minusMonths(1)}"
  }

  /** One export, optionally narrowed to a segment of it.
    *
    * The segment travels with the path rather than being applied by the caller: a bare `List[String]` cannot express which part of a file a
    * run is entitled to, so nothing would stop a search seeing the data its champion is judged on.
    */
  final case class Dataset(filePath: String, range: Option[DateRange] = None) {
    def currencyPair: CurrencyPair = {
      val cpStr = filePath.slice(0, 7).replaceAll("-", "").toUpperCase()
      CurrencyPair.from(cpStr).toOption.getOrElse(throw new IllegalArgumentException(s"Invalid currency pair in file path: $filePath"))
    }
    def interval: Interval        = if (filePath.contains("1h")) Interval.H1 else Interval.D1
    override def toString: String = range.fold(filePath)(r => s"$filePath[$r]")
  }

  /** The data one search is entitled to: the segments it may score against, and the one segment its finalists are ranked on.
    *
    * The two travel together because the split between them is the only thing that makes a champion's validation figure mean anything, and
    * a caller holding them as separate arguments can pass the same segments twice — a search scored on its own ranking data, then reported
    * as though it had been held out.
    *
    * The holdout is deliberately not here. Anything a search is handed is something a search can read, and the holdout is only worth having
    * for as long as nothing has selected against it.
    */
  final case class Corpus(searchFolds: List[List[Dataset]], validationFold: List[Dataset] = Nil) {
    def foldCount: Int = searchFolds.size

    /** How a round's corpus is written into its report, so the shape of a split is described where the split is defined. */
    def describe: List[String] =
      searchFolds.zipWithIndex.map { case (fold, index) =>
        s"Searched fold ${index + 1} of $foldCount, ${fold.size} dataset(s): ${fold.mkString(", ")}"
      } :+ s"Ranked finalists on ${validationFold.size} dataset(s): ${validationFold.mkString(", ")}"
  }

  private val majorFiles1h = List(
    "aud-usd-1h-1year.csv",
    "eur-usd-1h-1year.csv",
    "gbp-usd-1h-1year.csv",
    "nzd-usd-1h-1year.csv",
    "usd-cad-1h-1year.csv",
    "usd-chf-1h-1year.csv"
  )

  private val majorFiles1h_202507_202606 = List(
    "aud-usd-1h-1year-2025-07-2026-06.csv",
    "eur-usd-1h-1year-2025-07-2026-06.csv",
    "gbp-usd-1h-1year-2025-07-2026-06.csv",
    "nzd-usd-1h-1year-2025-07-2026-06.csv",
    "usd-cad-1h-1year-2025-07-2026-06.csv",
    "usd-chf-1h-1year-2025-07-2026-06.csv"
  )

  /** The whole of the older export, 2024-07 to 2025-07. Fine for measuring a strategy that already exists; not for choosing one, since a
    * search that scores against this has nothing left to be checked against.
    */
  val majors1h: List[Dataset] = majorFiles1h.map(Dataset(_))

  /** The whole of the newer export, 2025-07 to 2026-06: the test set.
    *
    * Reserved for the final go/no-go on a strategy that has already been chosen. Selecting on it — picking between candidates by how they
    * score here, even once — spends it, and there is no third year to replace it with.
    */
  val majors1h_202507_202606: List[Dataset] = majorFiles1h_202507_202606.map(Dataset(_))

  /** How many calendar months one scored segment holds.
    *
    * Four, which divides each twelve-month export into exactly three folds. Every consistency threshold is counted in months, so a shorter
    * segment measures each of them on less: at three months a pair contributes only three monthly buckets, and the counting statistics over
    * so few are nothing like the same statistics over a year.
    */
  val segmentMonths: Int = 4

  /** One export carved into contiguous segments of `segmentMonths`, oldest first, each carrying every pair.
    *
    * Non-overlapping, because the segments are meant to be separate pieces of evidence: a bar in two of them is one stretch of market
    * counted twice, and a candidate fitted to it rewarded twice.
    */
  private def segmentsOf(files: List[String], from: YearMonth, until: YearMonth): List[List[Dataset]] =
    Iterator
      .iterate(from)(_.plusMonths(segmentMonths))
      .takeWhile(start => !start.plusMonths(segmentMonths).isAfter(until))
      .map(start => files.map(f => Dataset(f, Some(DateRange(start, start.plusMonths(segmentMonths))))))
      .toList

  /** The segments a search is allowed to score against, oldest first: the whole of the older export, three folds of four months.
    *
    * More than one on purpose. A candidate scored on a single stretch of market can win by fitting that stretch, and nothing in the fitness
    * tells that apart from an edge; scored across time-disjoint stretches it has to hold up in each. This does not make the fitness
    * out-of-sample — anything a search scores against is in-sample by definition — it makes one a single well-fitted regime cannot satisfy.
    *
    * The first fold opens on the file's first bar, so its first hundred bars are spent forming the first window and never offered, while
    * `coveredMonths` still bills that month whole. Five days of a four-month fold, and the alternative is giving a whole month of a
    * twelve-month export to warm-up — `read` needs exactly 99 bars of history and every window it emits holds 100.
    */
  val majors1hSearchFolds: List[List[Dataset]] = segmentsOf(majorFiles1h, YearMonth.of(2024, 7), YearMonth.of(2025, 7))

  /** The segment a search's finalists are ranked on, having never been scored against during the search itself.
    *
    * Drawn from the newer export, so it is a later regime than any fold rather than a later slice of the same one, and the same length as a
    * fold, so the month-counted thresholds mean the same thing on both and training and validation fitness stay comparable. It opens a
    * month into its file, so `read` hands it a full window of prior history.
    */
  val majors1hValidationFold: List[Dataset] = {
    val start = YearMonth.of(2025, 8)
    segmentsOf(majorFiles1h_202507_202606, start, start.plusMonths(segmentMonths)).head
  }

  /** The split every round searches against, which is why no round names its own. */
  val majors1hCorpus: Corpus = Corpus(majors1hSearchFolds, majors1hValidationFold)

  /** The last seven months, which nothing in `Optimiser` reads.
    *
    * The folds and the validation segment are both spent by the time a round finishes, so neither can say whether the champion generalises.
    * This is what is left to say it, and it says it once: measuring a strategy here is fine, choosing between strategies here is selection,
    * and there is no more data to check that against.
    */
  val majors1hHoldout: List[Dataset] =
    majorFiles1h_202507_202606.map(f => Dataset(f, Some(DateRange(YearMonth.of(2025, 12), YearMonth.of(2026, 7)))))

  /** The two CSV exports under resources, which differ in three ways at once.
    *
    * `Legacy` names its first column "Local time" and stamps every row with a UK offset that follows daylight saving, carries a row for
    * every hour of the calendar year whether the market was open or not — the closed ones padded with a volume of 0 — and reports volume in
    * millions of the base currency. `Utc` is ISO-8601 in UTC, omits the hours the market was shut, and reports volume in whole
    * base-currency units.
    *
    * Only two of those differences need handling. The zero-volume padding is already dropped by the volume filter in `read`, which the
    * newer files pass through untouched because no row of theirs reports zero volume.
    */
  private enum CsvFormat:
    case Legacy, Utc

  private def csvFormatOf(dateTimeStr: String): CsvFormat =
    if (dateTimeStr.contains(' ')) CsvFormat.Legacy else CsvFormat.Utc

  private val legacyFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss.SSSXXX")

  // Legacy volumes count millions of the base currency and the newer ones count whole units, which leaves the same
  // hour of the same market six orders of magnitude apart between the two exports. Normalising onto the legacy scale
  // rather than the other way round keeps a backtest over the old files reporting exactly the numbers it always has.
  private val utcVolumeToLegacyScale = 1_000_000.0

  def read[F[_]: Async](dataset: Dataset): Stream[F, MarketTimeSeriesData] =
    readClassResource[F, MarketDataProvider.type](s"/${dataset.filePath}")
      .through(text.utf8.decode)
      .through(text.lines)
      .drop(1)
      .filter(l => l.split(",")(5).toDouble > 0)
      .map { line =>
        val vals   = line.split(",")
        val format = csvFormatOf(vals(0))
        PriceRange(
          vals(1).toDouble,
          vals(2).toDouble,
          vals(3).toDouble,
          vals(4).toDouble,
          parseVolume(vals(5), format),
          parseDateTime(vals(0), format)
        )
      }
      .sliding(100)
      .map(_.toNel.map(prices => MarketTimeSeriesData(dataset.currencyPair, dataset.interval, prices.reverse, "csv")))
      .unNone
      .filter(data => dataset.range.forall(_.contains(data.latestTime)))

  private def parseDateTime(dateTimeStr: String, format: CsvFormat): Instant =
    format match
      case CsvFormat.Utc =>
        OffsetDateTime.parse(dateTimeStr).toInstant
      case CsvFormat.Legacy =>
        val withIsoOffset = dateTimeStr.replace(" GMT-0000", "Z").replace(" GMT+0100", "+01:00")
        ZonedDateTime.parse(withIsoOffset, legacyFormatter).toInstant

  private def parseVolume(volStr: String, format: CsvFormat): Double =
    format match
      case CsvFormat.Utc    => volStr.toDouble / utcVolumeToLegacyScale
      case CsvFormat.Legacy => volStr.toDouble
