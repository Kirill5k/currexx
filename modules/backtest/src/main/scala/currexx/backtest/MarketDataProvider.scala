package currexx.backtest

import cats.effect.Async
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import fs2.io.readClassResource
import fs2.{Stream, text}

import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime

object MarketDataProvider:
  val majors = List(
    "aud-usd-1d-2years.csv",
    "eur-usd-1d-2years.csv",
    "gbp-usd-1d-2years.csv",
    "nzd-usd-1d-2years.csv",
    "usd-cad-1d-2years.csv",
    "usd-chf-1d-2years.csv"
  )

  val majors1h = List(
    "aud-usd-1h-1year.csv",
    "eur-usd-1h-1year.csv",
    "gbp-usd-1h-1year.csv",
    "nzd-usd-1h-1year.csv",
    "usd-cad-1h-1year.csv",
    "usd-chf-1h-1year.csv"
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
    "eur-aud-1d.csv"
  )

  private val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss.SSSXXX")

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
        val vals = line.split(",")
        PriceRange(
          vals(1).toDouble,
          vals(2).toDouble,
          vals(3).toDouble,
          vals(4).toDouble,
          vals(5).toDouble,
          ZonedDateTime.parse(vals(0).replace(" GMT-0000", "Z").replace(" GMT+0100", "+01:00"), formatter).toInstant
        )
      }
      .sliding(100)
      .map(_.toNel.map(prices => MarketTimeSeriesData(cp, interval, prices.reverse)))
      .unNone
  }
