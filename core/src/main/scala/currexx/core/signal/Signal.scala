package currexx.core.signal

import currexx.core.common.validations.CurrencyPairString
import squants.market.{Currency, defaultMoneyContext}

final case class CurrencyPair(base: Currency, quote: Currency)
object CurrencyPair:
  def from(cp: CurrencyPairString): CurrencyPair = {
    val Array(base, quote) = cp.value.split("/")
    CurrencyPair(defaultMoneyContext.currencyMap(base), defaultMoneyContext.currencyMap(quote))
  }

sealed trait Signal:
  def currencyPair: CurrencyPair
