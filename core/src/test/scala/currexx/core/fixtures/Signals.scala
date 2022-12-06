package currexx.core.fixtures

import currexx.core.signal.*
import currexx.domain.market.{Condition, Direction, Indicator, ValueSource, ValueTransformation}
import currexx.domain.market.Currency.{EUR, GBP}

import java.time.Instant
import java.time.temporal.ChronoField

object Signals {
  lazy val ts = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)

  lazy val trendDirectionChanged = Signal(
    Users.uid,
    Markets.gbpeur,
    Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(1)),
    Markets.trendChangeDetection,
    ts
  )

  lazy val thresholdCrossing = Signal(
    Users.uid,
    Markets.gbpeur,
    Condition.AboveThreshold(BigDecimal(80), 95),
    Markets.thresholdCrossing,
    ts
  )
}
