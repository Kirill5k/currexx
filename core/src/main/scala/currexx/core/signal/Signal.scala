package currexx.core.signal

import squants.market.Currency

final case class CurrencyPair(base: Currency, quote: Currency)

sealed trait Signal:
  def currencyPair: CurrencyPair
