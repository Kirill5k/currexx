package currexx.core.trade

import currexx.clients.broker.BrokerParameters
import currexx.domain.user.UserId
import currexx.domain.market.TradeOrder
import io.circe.Codec

final case class TradingParameters(
    volume: BigDecimal,
    stopLoss: Option[BigDecimal],
    trailingStopLoss: Option[BigDecimal],
    takeProfit: Option[BigDecimal]
) derives Codec.AsObject:
  def toOrder(position: TradeOrder.Position): TradeOrder =
    TradeOrder.Enter(position, volume, stopLoss, trailingStopLoss, takeProfit)

final case class TradeSettings(
    userId: UserId,
    strategy: TradeStrategy,
    broker: BrokerParameters,
    trading: TradingParameters
)
