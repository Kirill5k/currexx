package currexx.core.signal

import currexx.domain.market.{CurrencyPair, Indicator, IndicatorParameters}
import currexx.domain.user.UserId

final case class SignalSettings(
    userId: UserId,
    indicators: List[IndicatorParameters]
)

object SignalSettings:
  def default(userId: UserId): SignalSettings = 
    SignalSettings(userId, List(IndicatorParameters.MACD(), IndicatorParameters.RSI()))
