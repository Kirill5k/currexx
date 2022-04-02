package currexx.core.fixtures

import currexx.core.signal.*
import squants.market.{EUR, GBP}

import java.time.Instant

object Signals {
  lazy val ts     = Instant.now
  lazy val gbpeur = CurrencyPair(GBP, EUR)
  lazy val macd   = Signal(Users.uid, gbpeur, Indicators.macd, ts)
}
