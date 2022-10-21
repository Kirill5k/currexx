package currexx.backtest

import cats.effect.Sync
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}

import java.nio.file.Paths
import java.time.{Instant, LocalDate, ZoneOffset}
import java.time.format.DateTimeFormatter

object MarketDataProvider:
  val completeDataset = List(
    "aud-cad-1d.csv",
    "aud-jpy-1d.csv",
    "cad-jpy-1d.csv",
    "eur-aud-1d.csv",
    "eur-chf-1d.csv",
    "eur-gbp-1d.csv",
    "gbp-jpy-1d.csv",
    "nzd-cad-1d.csv",
    "nzd-chf-1d.csv",
    "nzd-jpy-1d.csv",
    "usd-dkk-1d.csv",
    "usd-jpy-1d.csv",
    "usd-nok-1d.csv",
    "usd-pln-1d.csv"
  )

  val euusDataset = List(
    "aud-cad-1d.csv",
    "eur-aud-1d.csv",
    "eur-chf-1d.csv",
    "eur-gbp-1d.csv",
    "nzd-cad-1d.csv",
    "nzd-chf-1d.csv",
    "usd-dkk-1d.csv"
  )
  
  val audDataset = List(
    "aud-cad-1d.csv",
    "eur-aud-1d.csv",
  )
  
  private val timePattern = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  
  def read[F[_]: Sync: Files](
      filePath: String,
      currencyPair: CurrencyPair,
      interval: Interval = Interval.D1
  ): Stream[F, MarketTimeSeriesData] =
    Files[F]
      .readAll(Path(getClass.getClassLoader.getResource(filePath).getPath))
      .through(text.utf8.decode)
      .through(text.lines)
      .drop(1)
      .map { line =>
        val vals = line.split(",")
        PriceRange(
          vals(1).toDouble,
          vals(2).toDouble,
          vals(3).toDouble,
          vals(4).toDouble,
          vals(5).toDouble,
          LocalDate.parse(vals(0).subSequence(0, 10), timePattern).atStartOfDay().atOffset(ZoneOffset.UTC).toInstant
        )
      }
      .sliding(100)
      .map(_.toNel.map(prices => MarketTimeSeriesData(currencyPair, interval, prices.reverse)))
      .unNone
