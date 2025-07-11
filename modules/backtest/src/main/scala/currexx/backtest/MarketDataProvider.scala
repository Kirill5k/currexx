package currexx.backtest

import cats.effect.Async
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import fs2.io.file.{Files, Path}
import fs2.{Stream, text}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneOffset}

//TODO: update data points + find 1h
object MarketDataProvider:
  val majors = List(
    "aud-usd-1d-2years.csv",
    "eur-usd-1d-2years.csv",
    "gbp-usd-1d-2years.csv",
    "nzd-usd-1d-2years.csv",
    "usd-cad-1d-2years.csv",
    "usd-chf-1d-2years.csv"
  )
  
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
  
  def read[F[_]: Async](
      filePath: String,
      currencyPair: CurrencyPair,
      interval: Interval = Interval.D1
  ): Stream[F, MarketTimeSeriesData] =
    Files
      .forAsync[F]
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
