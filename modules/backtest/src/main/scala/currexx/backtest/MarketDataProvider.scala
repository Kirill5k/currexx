package currexx.backtest

import cats.effect.Async
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import fs2.io.readClassResource
import fs2.{Stream, text}

import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZonedDateTime}

object MarketDataProvider:
  val majors1h = List(
    "aud-usd-1h-1year.csv",
    "eur-usd-1h-1year.csv",
    "gbp-usd-1h-1year.csv",
    "nzd-usd-1h-1year.csv",
    "usd-cad-1h-1year.csv",
    "usd-chf-1h-1year.csv"
  )

  val majors1h_202507_202606 = List(
    "aud-usd-1h-1year-2025-07-2026-06.csv",
    "eur-usd-1h-1year-2025-07-2026-06.csv",
    "gbp-usd-1h-1year-2025-07-2026-06.csv",
    "nzd-usd-1h-1year-2025-07-2026-06.csv",
    "usd-cad-1h-1year-2025-07-2026-06.csv",
    "usd-chf-1h-1year-2025-07-2026-06.csv"
  )

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

  def read[F[_]: Async](filePath: String): Stream[F, MarketTimeSeriesData] = {
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
