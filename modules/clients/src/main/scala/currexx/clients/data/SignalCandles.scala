package currexx.clients.data

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.domain.market.{Interval, MarketTimeSeriesData}
import kirill5k.common.syntax.time.*
import org.typelevel.log4cats.Logger

import java.time.Instant

private[data] object SignalCandles {
  def prepare[F[_]](
      data: MarketTimeSeriesData,
      interval: Interval,
      now: Instant,
      signalCandleCount: Int
  )(using F: Monad[F], logger: Logger[F]): F[MarketTimeSeriesData] = {
    val candleEndTime = data.prices.head.time.plus(interval.toDuration)
    val shouldExclude = candleEndTime.isAfter(now)
    val filtered      =
      if (shouldExclude && data.prices.size > 1)
        F.pure(data.copy(prices = NonEmptyList.fromListUnsafe(data.prices.tail)))
      else if (shouldExclude && data.prices.size == 1)
        logger.warn(s"Only incomplete candle available for ${data.currencyPair} at ${data.interval}") >> F.pure(data)
      else F.pure(data)

    filtered.map(result => result.copy(prices = NonEmptyList.fromListUnsafe(result.prices.take(signalCandleCount))))
  }
}
