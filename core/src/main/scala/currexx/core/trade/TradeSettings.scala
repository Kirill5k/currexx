package currexx.core.trade

import currexx.clients.broker.BrokerParameters
import currexx.domain.user.UserId
import currexx.domain.market.{CurrencyPair, TradeOrder}
import io.circe.Codec

final case class TradingParameters(
    volume: BigDecimal,
    stopLoss: Option[BigDecimal] = None,
    stopLossPerCurrency: Map[String, BigDecimal] = Map.empty,
    trailingStopLoss: Option[BigDecimal] = None,
    takeProfit: Option[BigDecimal] = None
) derives Codec.AsObject:
  def toOrder(pair: CurrencyPair, position: TradeOrder.Position): TradeOrder =
    TradeOrder.Enter(position, volume, stopLossPerCurrency.get(pair.quote.code).orElse(stopLoss), trailingStopLoss, takeProfit)

final case class TradeSettings(
    userId: UserId,
    strategy: TradeStrategy,
    broker: BrokerParameters,
    trading: TradingParameters
)
