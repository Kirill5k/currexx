package currexx.core.fixtures

import currexx.core.signal.*
import currexx.domain.market.Interval
import currexx.domain.signal.{Condition, Direction, Indicator, ValueSource, ValueTransformation}

import java.time.Instant
import java.time.temporal.ChronoField

object Signals {
  lazy val ts = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)

  
  lazy val trendChangeDetection: Indicator =
    Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(16))
  lazy val thresholdCrossing: Indicator =
    Indicator.ThresholdCrossing(ValueSource.Close, ValueTransformation.STOCH(14), 80d, 20d)

  lazy val trendDirectionChanged: Signal = Signal(
    userId = Users.uid,
    currencyPair = Markets.gbpeur,
    interval = Interval.H1,
    condition = Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(1)),
    triggeredBy = trendChangeDetection,
    time = ts
  )

  lazy val thresholdCrossed: Signal = Signal(
    userId = Users.uid,
    currencyPair = Markets.gbpeur,
    interval = Interval.H1,
    condition = Condition.AboveThreshold(BigDecimal(80), 95),
    triggeredBy = thresholdCrossing,
    time = ts
  )
}
