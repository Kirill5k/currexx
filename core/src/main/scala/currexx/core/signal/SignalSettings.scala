package currexx.core.signal

import currexx.domain.market.{CurrencyPair, Indicator, IndicatorParameters}
import currexx.domain.user.UserId

final case class SignalSettings(
    userId: UserId,
    currencyPair: CurrencyPair,
    indicators: List[IndicatorParameters]
)
