package currexx.core.fixtures

import currexx.core.signal.*
import currexx.domain.market.{Condition, Indicator, Trend, ValueSource, ValueTransformation}
import squants.market.{EUR, GBP}

import java.time.Instant
import java.time.temporal.ChronoField

object Signals {
  lazy val ts = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)

  lazy val trendDirectionChanged = Signal(
    Users.uid,
    Markets.gbpeur,
    Condition.TrendDirectionChange(Trend.Downward, Trend.Upward),
    Markets.trendChangeDetection,
    ts
  )

  lazy val settings = SignalSettings(Users.uid, TriggerFrequency.OncePerDay, List(Markets.trendChangeDetection))
}
