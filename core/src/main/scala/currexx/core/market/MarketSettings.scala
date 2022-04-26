package currexx.core.market

import currexx.clients.broker.BrokerParameters
import currexx.domain.user.UserId

final case class TradingParameters(
    volume: BigDecimal,
    stopLoss: Option[BigDecimal],
    trailingStopLoss: Option[BigDecimal],
    takeProfit: Option[BigDecimal]
)

final case class MarketSettings(
    userId: UserId,
    broker: BrokerParameters,
    trading: TradingParameters
)
