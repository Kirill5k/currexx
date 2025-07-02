package currexx.core.fixtures

import currexx.core.signal.*
import currexx.domain.market.{Condition, Direction, Interval}

import java.time.Instant
import java.time.temporal.ChronoField

object Signals {
  lazy val ts = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)

  lazy val trendDirectionChanged = Signal(
    userId = Users.uid,
    currencyPair = Markets.gbpeur,
    interval = Interval.H1,
    condition = Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(1)),
    triggeredBy = Markets.trendChangeDetection,
    time = ts
  )

  lazy val thresholdCrossing = Signal(
    userId = Users.uid,
    currencyPair = Markets.gbpeur,
    interval = Interval.H1,
    condition = Condition.AboveThreshold(BigDecimal(80), 95),
    triggeredBy = Markets.thresholdCrossing,
    time = ts
  )
}
