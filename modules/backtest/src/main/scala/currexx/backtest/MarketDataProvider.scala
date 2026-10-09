package currexx.backtest

import cats.data.NonEmptyList
import cats.effect.{Async, Ref}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import fs2.io.readClassResource
import fs2.{Pipe, Stream, text}

import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, YearMonth, ZoneOffset, ZonedDateTime}
import java.security.MessageDigest
import java.util.HexFormat

object MarketDataProvider:

  val priceWindowSize: Int = 100

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

  /** One pair's chronological exports, optionally narrowed to a segment of their combined history.
    *
    * The segment travels with the path rather than being applied by the caller: a bare `List[String]` cannot express which part of a file a
    * run is entitled to, so nothing would stop a search seeing the data its champion is judged on.
    */
  final case class Dataset(filePaths: NonEmptyList[String], range: Option[DateRange]) {
    def currencyPair: CurrencyPair = currencyPairOf(filePaths.head)
    def interval: Interval         = intervalOf(filePaths.head)
    override def toString: String  = {
      val paths = filePaths.toList.mkString(" + ")
      range.fold(paths)(r => s"$paths[$r]")
    }
  }

  object Dataset {
    def apply(filePath: String, range: Option[DateRange] = None): Dataset =
      new Dataset(NonEmptyList.one(filePath), range)

    def apply(filePaths: NonEmptyList[String]): Dataset = new Dataset(filePaths, None)
  }

  /** Raw trading-bar availability; this metadata contains no prices, strategy results or sliding windows. */
  final case class HistoryCoverage(
      currencyPair: CurrencyPair,
      interval: Interval,
      firstBar: Instant,
      lastBar: Instant,
      perMonth: Map[YearMonth, Long]
  ) {
    def countBefore(month: YearMonth): Long = perMonth.iterator.collect { case (observed, count) if observed.isBefore(month) => count }.sum

    def countIn(range: DateRange): Long =
      perMonth.iterator.collect {
        case (month, count) if !month.isBefore(range.from) && month.isBefore(range.until) => count
      }.sum
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

  private val majorFiles1h_202307_202406 = List(
    "aud-usd-1h-1year-2023-07-2024-06.csv",
    "eur-usd-1h-1year-2023-07-2024-06.csv",
    "gbp-usd-1h-1year-2023-07-2024-06.csv",
    "nzd-usd-1h-1year-2023-07-2024-06.csv",
    "usd-cad-1h-1year-2023-07-2024-06.csv",
    "usd-chf-1h-1year-2023-07-2024-06.csv"
  )

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

  /** The whole of the oldest export, 2023-07 to 2024-06. Searched, like `majors1h`. */
  val majors1h_202307_202406: List[Dataset] = majorFiles1h_202307_202406.map(Dataset(_))

  /** The whole of the middle export, 2024-07 to 2025-06. Fine for measuring a strategy that already exists; not for choosing one, since a
    * search that scores against this has nothing left to be checked against.
    */
  val majors1h: List[Dataset] = majorFiles1h.map(Dataset(_))

  /** Everything the search folds cover, as whole files rather than segments: both older exports, 2023-07 to 2025-06.
    *
    * What "in sample" means once the folds span two exports. Measuring over either export alone would report half the data a champion was
    * chosen on and label it as all of it, which is the mislabelling this exists to prevent.
    */
  val majors1hSearched: List[Dataset] = majors1h_202307_202406 ::: majors1h

  /** The whole of the newest export, 2025-07 to 2026-06.
    *
    * Not the test set, despite reading like one: `majors1hValidationFold` is carved out of it, so four of these twelve months are what
    * every champion's finalist ranking selected on. Measuring here mixes selection and evaluation months and reports the blend as
    * out-of-sample. `majors1hHoldout` isolates the originally reserved evaluation period; s10_v2 has now reused it for manual development.
    */
  val majors1h_202507_202606: List[Dataset] = majorFiles1h_202507_202606.map(Dataset(_))

  /** One uninterrupted series per pair, July 2023 through June 2026. Requested ranges also retain preceding price history for warm-up. */
  val majors1hHistory: List[Dataset] =
    majorFiles1h_202307_202406
      .zip(majorFiles1h)
      .zip(majorFiles1h_202507_202606)
      .map { case ((oldest, middle), newest) => Dataset(NonEmptyList.of(oldest, middle, newest)) }

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

  /** The segments a search is allowed to score against, oldest first: both older exports, six folds of four months spanning two years.
    *
    * More than one on purpose. A candidate scored on a single stretch of market can win by fitting that stretch, and nothing in the fitness
    * tells that apart from an edge; scored across time-disjoint stretches it has to hold up in each. This does not make the fitness
    * out-of-sample — anything a search scores against is in-sample by definition — it makes one a single well-fitted regime cannot satisfy.
    *
    * Six rather than three because three did not refuse enough. Over one contiguous year the folds are three slices of one regime, and the
    * 2026-08-24/25 rounds show what that buys: a median of 23% of the training score retained on validation, six of sixteen rounds finding
    * nothing above zero at all. `FoldAggregation` is a geometric mean that zeroes if any fold fails, so folds are an AND — spanning them
    * across two years asks a candidate to hold up in two regimes rather than in three views of one.
    *
    * Each export is segmented on its own boundaries rather than as one continuous span, because the two are separate files: a fold may not
    * straddle them. Both are exactly twelve months, so each divides into three whole folds with no remainder.
    *
    * The first fold of each export opens on that file's first bar, so its first hundred bars are spent forming the first window and never
    * offered, while `coveredMonths` still bills that month whole. Five days of a four-month fold, and the alternative is giving a whole
    * month of a twelve-month export to warm-up — `read` needs exactly 99 bars of history and every window it emits holds 100.
    */
  val majors1hSearchFolds: List[List[Dataset]] =
    segmentsOf(majorFiles1h_202307_202406, YearMonth.of(2023, 7), YearMonth.of(2024, 7)) :::
      segmentsOf(majorFiles1h, YearMonth.of(2024, 7), YearMonth.of(2025, 7))

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
    * and there is no more data to check that against. The manual s10_v2 follow-up did reuse this period for selection; its reports label it
    * historical development data, not a fresh holdout.
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

  final private case class ReadProgress(months: Set[YearMonth] = Set.empty, reachedCutoff: Boolean = false) {
    def observe(price: PriceRange): ReadProgress = copy(months = months + monthOf(price.time))
  }

  def read[F[_]: Async](dataset: Dataset): Stream[F, MarketTimeSeriesData] =
    Stream.eval(Ref.of[F, ReadProgress](ReadProgress())).flatMap { progress =>
      val prices = rawPrices[F](dataset)
        .through(beforePeriodEnd(dataset, progress))
        .through(orderedTradingPrices(dataset))
        .evalTap(price => progress.update(_.observe(price)))
      val checkCoverage = Stream.eval(progress.get).map(state => validateCoverage(dataset, state.months)).rethrow.drain

      (prices ++ checkCoverage).through(priceWindows(dataset, progress))
    }

  private def rawPrices[F[_]: Async](dataset: Dataset): Stream[F, PriceRange] =
    Stream
      .eval(Async[F].delay(validateDataset(dataset)))
      .rethrow
      .flatMap(_ => Stream.emits(dataset.filePaths.toList).flatMap(readPrices[F]))

  private def beforePeriodEnd[F[_]: Async](dataset: Dataset, progress: Ref[F, ReadProgress]): Pipe[F, PriceRange, PriceRange] =
    val end = dataset.range.map(_.until.atDay(1).atStartOfDay(ZoneOffset.UTC).toInstant)
    _.evalMap { price =>
      if (end.forall(price.time.isBefore)) Async[F].pure(Option(price))
      else progress.update(_.copy(reachedCutoff = true)).as(Option.empty[PriceRange])
    }.unNoneTerminate

  private def orderedTradingPrices[F[_]: Async](dataset: Dataset): Pipe[F, PriceRange, PriceRange] =
    _.zipWithPrevious
      .map { case (previous, price) =>
        Either.cond(
          previous.forall(_.time.isBefore(price.time)),
          price,
          new IllegalArgumentException(s"Dataset timestamps must be strictly increasing: $dataset at ${price.time}")
        )
      }
      .rethrow
      .filter(_.volume > 0)

  private def priceWindows[F[_]: Async](dataset: Dataset, progress: Ref[F, ReadProgress]): Pipe[F, PriceRange, MarketTimeSeriesData] =
    _.sliding(priceWindowSize)
      // Preserve short exports, but do not invent a partial window when a date cutoff interrupts warm-up.
      .evalFilter(prices => if (prices.size == priceWindowSize) Async[F].pure(true) else progress.get.map(!_.reachedCutoff))
      .map(_.toNel.map(prices => MarketTimeSeriesData(dataset.currencyPair, dataset.interval, prices.reverse, "csv")))
      .unNone
      .filter(data => dataset.range.forall(_.contains(data.latestTime)))

  /** SHA-256 of the unmodified CSV bytes, independent of parsing and selected date ranges. */
  def fingerprint[F[_]: Async](filePath: String): F[String] =
    Async[F].delay(MessageDigest.getInstance("SHA-256")).flatMap { digest =>
      readClassResource[F, MarketDataProvider.type](s"/$filePath").chunks
        .evalMap(chunk => Async[F].delay(digest.update(chunk.toArray)))
        .compile
        .drain
        .flatMap(_ => Async[F].delay(HexFormat.of().formatHex(digest.digest())))
    }

  /** Inspect the unranged history supplied by preflight. Only timestamps and positive-volume bar counts are retained. */
  def inspectHistory[F[_]: Async](dataset: Dataset): F[HistoryCoverage] =
    rawPrices[F](dataset)
      .through(orderedTradingPrices(dataset))
      .compile
      .fold(Option.empty[HistoryCoverage])((coverage, price) => Some(recordCoverage(dataset, coverage, price.time)))
      .flatMap(coverage => Async[F].fromOption(coverage, new IllegalArgumentException(s"History contains no trading bars: $dataset")))

  private def recordCoverage(dataset: Dataset, coverage: Option[HistoryCoverage], time: Instant): HistoryCoverage =
    val month = monthOf(time)
    coverage match
      case None           => HistoryCoverage(dataset.currencyPair, dataset.interval, time, time, Map(month -> 1L))
      case Some(previous) =>
        previous.copy(lastBar = time, perMonth = previous.perMonth.updated(month, previous.perMonth.getOrElse(month, 0L) + 1L))

  private def monthOf(time: Instant): YearMonth = YearMonth.from(time.atOffset(ZoneOffset.UTC))

  private def currencyPairOf(filePath: String): CurrencyPair = {
    val cpStr = filePath.slice(0, 7).replaceAll("-", "").toUpperCase()
    CurrencyPair.from(cpStr).toOption.getOrElse(throw new IllegalArgumentException(s"Invalid currency pair in file path: $filePath"))
  }

  private def intervalOf(filePath: String): Interval = if (filePath.contains("1h")) Interval.H1 else Interval.D1

  private def validateDataset(dataset: Dataset): Either[IllegalArgumentException, Unit] =
    if (dataset.filePaths.exists(path => currencyPairOf(path) != dataset.currencyPair))
      Left(new IllegalArgumentException(s"Dataset mixes currency pairs: $dataset"))
    else if (dataset.filePaths.exists(path => intervalOf(path) != dataset.interval))
      Left(new IllegalArgumentException(s"Dataset mixes intervals: $dataset"))
    else if (dataset.range.exists(range => !range.from.isBefore(range.until)))
      Left(new IllegalArgumentException(s"Dataset range must be nonempty and increasing: $dataset"))
    else Right(())

  private def validateCoverage(dataset: Dataset, observedMonths: Set[YearMonth]): Either[IllegalArgumentException, Unit] =
    val missing = dataset.range.toList.flatMap { range =>
      Iterator.iterate(range.from)(_.plusMonths(1)).takeWhile(_.isBefore(range.until)).filterNot(observedMonths).toList
    }
    Either.cond(
      missing.isEmpty,
      (),
      new IllegalArgumentException(s"Dataset has no trading bars in requested month(s) ${missing.mkString(", ")}: $dataset")
    )

  private def readPrices[F[_]: Async](filePath: String): Stream[F, PriceRange] =
    readClassResource[F, MarketDataProvider.type](s"/$filePath")
      .through(text.utf8.decode)
      .through(text.lines)
      .drop(1)
      .filter(_.nonEmpty)
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
