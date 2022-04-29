package currexx.core.fixtures

import currexx.domain.market.*
import currexx.core.signal.*
import squants.market.{EUR, GBP}

import java.time.Instant
import java.time.temporal.ChronoField

object Signals {
  lazy val ts   = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)
  lazy val macd = Signal(Users.uid, Markets.gbpeur, Indicator.MACD, Condition.CrossingUp, ts)
  lazy val rsi = Signal(Users.uid, Markets.gbpeur, Indicator.RSI, Condition.AboveThreshold(BigDecimal(80), BigDecimal(85)), ts)

  lazy val settings = SignalSettings(Users.uid, List(IndicatorParameters.MACD(), IndicatorParameters.RSI()))
}
