package currexx.core.fixtures

import currexx.core.signal.*
import squants.market.{EUR, GBP}

import java.time.Instant
import java.time.temporal.ChronoField

object Signals {
  lazy val ts     = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)
  lazy val gbpeur = CurrencyPair(GBP, EUR)
  lazy val macd   = Signal(Users.uid, gbpeur, Indicators.macd, ts)
}
