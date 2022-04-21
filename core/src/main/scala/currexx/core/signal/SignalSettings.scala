package currexx.core.signal

import currexx.domain.market.{CurrencyPair, Indicator, IndicatorParameters}
import currexx.domain.user.UserId

final case class SignalSettings(
    userId: UserId,
    currencyPair: CurrencyPair,
    indicators: List[IndicatorParameters]
)

object SignalSettings:
  def default(userId: UserId, currencyPair: CurrencyPair): SignalSettings =
    SignalSettings(userId, currencyPair, List(IndicatorParameters.MACD()))
