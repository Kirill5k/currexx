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
    * Months rather than instants because everything downstream that judges consistency counts calendar months, so a boundary falling
    * mid-month would hand the segments either side of it a stub month that was never offered a full month's trading and is then scored as
    * though it had been.
    */
  final case class DateRange(from: YearMonth, until: YearMonth) {
    private val fromTime  = from.atDay(1).atStartOfDay(ZoneOffset.UTC).toInstant
    private val untilTime = until.atDay(1).atStartOfDay(ZoneOffset.UTC).toInstant

    def contains(time: Instant): Boolean = !time.isBefore(fromTime) && time.isBefore(untilTime)
    override def toString: String        = s"$from..${until.minusMonths(1)}"
  }

  /** One export, optionally narrowed to a segment of it.
    *
    * The segment travels with the path rather than being applied by the caller, because the whole point of the split is that a search must
    * not see the data its champion is judged on. A `List[String]` of paths cannot express which part of a file a run is entitled to, so
    * nothing stops the wrong slice being passed; this can only be got wrong by naming the wrong value.
    */
  final case class Dataset(filePath: String, range: Option[DateRange] = None) {
    def currencyPair: CurrencyPair = cpFromFilePath(filePath)
    override def toString: String  = range.fold(filePath)(r => s"$filePath[$r]")
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

  // The older export split down the middle, six months each way. July 2024 is a partial month, because the data starts
  // on the 7th; it is left in the training half, where a short month costs nothing, rather than in the half that has to
  // answer for what it earned per month.
  private val trainingRange   = DateRange(YearMonth.of(2024, 7), YearMonth.of(2025, 1))
  private val validationRange = DateRange(YearMonth.of(2025, 1), YearMonth.of(2025, 7))

  /** The half a search is allowed to score against. */
  val majors1hTraining: List[Dataset] = majorFiles1h.map(f => Dataset(f, Some(trainingRange)))

  /** The half a search's finalists are ranked on, having never been scored against during the search itself.
    *
    * Six months rather than a token slice, so that the same consistency thresholds a candidate was scored against still mean something
    * here: a validation segment shorter than `minMonthsCovered` would discount every candidate identically and report a breach none of them
    * could have avoided.
    */
  val majors1hValidation: List[Dataset] = majorFiles1h.map(f => Dataset(f, Some(validationRange)))

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

  def cpFromFilePath(filePath: String): CurrencyPair =
    val cpStr = filePath.slice(0, 7).replaceAll("-", "").toUpperCase()
    CurrencyPair.from(cpStr).toOption.getOrElse(throw new IllegalArgumentException(s"Invalid currency pair in file path: $filePath"))

  def read[F[_]: Async](dataset: Dataset): Stream[F, MarketTimeSeriesData] = {
    val filePath = dataset.filePath
    val interval = if (filePath.contains("1h")) Interval.H1 else Interval.D1
    val cp       = cpFromFilePath(filePath)
    readClassResource[F, MarketDataProvider.type](s"/$filePath")
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
      .map(_.toNel.map(prices => MarketTimeSeriesData(cp, interval, prices.reverse, "csv")))
      .unNone
      .filter(data => dataset.range.forall(_.contains(data.latestTime)))
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
