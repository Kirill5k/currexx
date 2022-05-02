package currexx.backtest

import cats.effect.Sync
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}

import java.time.{Instant, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.Date

object TestDataProvider {

  private val timePattern = DateTimeFormatter.ofPattern( "dd.MM.yyyy hh:mm:ss")

  def read[F[_]: Sync: Files](
      currencyPair: CurrencyPair,
      interval: Interval,
      filePath: String
  ): Stream[F, MarketTimeSeriesData] =
    Files[F]
      .readAll(Path(filePath))
      .through(text.utf8.decode)
      .through(text.lines)
      .drop(1)
      .map { line =>
        val vals = line.split(",")
        PriceRange(
          BigDecimal(vals(1)),
          BigDecimal(vals(2)),
          BigDecimal(vals(3)),
          BigDecimal(vals(4)),
          BigDecimal(vals(5)),
          ZonedDateTime.parse(vals(0).subSequence(0, 18), timePattern).toInstant
        )
      }
      .sliding(100)
      .map(_.toNel.map(prices => MarketTimeSeriesData(currencyPair, interval, prices.reverse)))
      .unNone
}
