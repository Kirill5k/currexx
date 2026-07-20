package currexx.backtest

import cats.effect.Async
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import fs2.io.readClassResource
import fs2.{Stream, text}

import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime

object MarketDataProvider:
  val majors1h = List(
    "aud-usd-1h-1year.csv",
    "eur-usd-1h-1year.csv",
    "gbp-usd-1h-1year.csv",
    "nzd-usd-1h-1year.csv",
    "usd-cad-1h-1year.csv",
    "usd-chf-1h-1year.csv"
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
      .map(_.toNel.map(prices => MarketTimeSeriesData(cp, interval, prices.reverse, "csv")))
      .unNone
  }
