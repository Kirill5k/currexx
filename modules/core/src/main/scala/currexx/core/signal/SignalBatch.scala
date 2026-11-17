package currexx.core.signal

import currexx.domain.market.{Interval, MarketTimeSeriesData}
import currexx.domain.signal.{Indicator, IndicatorId}
import kirill5k.common.syntax.time.*

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** A complete candle result, including the absence of emitted events. */
final case class SignalBatch(
    interval: Interval,
    revision: Long,
    candleTime: Instant,
    closureGap: Option[FiniteDuration],
    sources: Map[IndicatorId, Indicator],
    signals: List[Signal]
)

object SignalBatch:
  def closureGap(data: MarketTimeSeriesData): Option[FiniteDuration] =
    val interval = data.interval.toDuration
    data.prices.tail.headOption.map(_.time.durationBetween(data.latestTime)).filter(_ > interval * 2).map(_ - interval)
