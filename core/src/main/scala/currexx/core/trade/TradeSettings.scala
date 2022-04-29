package currexx.core.trade

import currexx.clients.broker.BrokerParameters
import currexx.domain.user.UserId
import io.circe.Codec

final case class TradingParameters(
    volume: BigDecimal,
    stopLoss: Option[BigDecimal],
    trailingStopLoss: Option[BigDecimal],
    takeProfit: Option[BigDecimal]
) derives Codec.AsObject

final case class TradeSettings(
    userId: UserId,
    broker: BrokerParameters,
    trading: TradingParameters
)
