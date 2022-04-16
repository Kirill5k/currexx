package currexx.core.fixtures

import squants.market.{EUR, GBP, USD}
import currexx.domain.market.CurrencyPair

object Markets {
  lazy val gbpeur = CurrencyPair(GBP, EUR)
  lazy val gbpusd = CurrencyPair(GBP, USD)
}
