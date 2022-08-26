package currexx.core.settings

import currexx.clients.broker.BrokerParameters
import currexx.core.signal.TriggerFrequency
import currexx.core.trade.{TradeStrategy, TradingParameters}
import currexx.domain.market.Indicator
import currexx.domain.user.UserId
import io.circe.Codec

final case class SignalParameters(
    triggerFrequency: TriggerFrequency,
    indicators: List[Indicator]
) derives Codec.AsObject

final case class TradeParameters(
    strategy: TradeStrategy,
    broker: BrokerParameters,
    trading: TradingParameters,
    comment: Option[String]
) derives Codec.AsObject

final case class GlobalSettings(
    userId: UserId,
    signal: Option[SignalParameters],
    trade: Option[TradeParameters]
)
