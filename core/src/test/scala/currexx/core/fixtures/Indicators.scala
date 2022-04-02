package currexx.core.fixtures

import currexx.core.signal.{Direction, Indicator}

object Indicators {
  lazy val macd = Indicator.MACD(Direction.Down, BigDecimal(0.4))
}
