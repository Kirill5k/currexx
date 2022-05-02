package currexx.backtest

import cats.effect.Sync
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}

import java.nio.file.Paths
import java.time.{Instant, LocalDate, ZoneOffset}
import java.time.format.DateTimeFormatter

object TestDataProvider {

  private val timePattern = DateTimeFormatter.ofPattern( "dd.MM.yyyy")

  def read[F[_]: Sync: Files](
      currencyPair: CurrencyPair,
      interval: Interval,
      filePath: String
  ): Stream[F, MarketTimeSeriesData] =
    Files[F]
      .readAll(Path(getClass.getClassLoader.getResource(filePath).getPath))
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
          LocalDate.parse(vals(0).subSequence(0, 10), timePattern).atStartOfDay().atOffset(ZoneOffset.UTC).toInstant
        )
      }
      .sliding(100)
      .map(_.toNel.map(prices => MarketTimeSeriesData(currencyPair, interval, prices.reverse)))
      .unNone
}
