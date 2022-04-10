package currexx.core.fixtures

import squants.market.{EUR, GBP}
import currexx.domain.market.CurrencyPair

object Markets {
  lazy val gbpeur = CurrencyPair(GBP, EUR)
}
