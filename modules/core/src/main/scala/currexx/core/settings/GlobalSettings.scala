package currexx.core.settings

import currexx.clients.broker.BrokerParameters
import currexx.core.trade.TradeStrategy
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.Indicator
import currexx.domain.user.UserId
import io.circe.Codec

final case class SignalSettings(
    indicators: List[Indicator]
) derives Codec.AsObject

final case class TradingParameters(
    volume: BigDecimal,
    stopLoss: Option[BigDecimal] = None,
    stopLossPerCurrency: Map[String, BigDecimal] = Map.empty,
    trailingStopLoss: Option[BigDecimal] = None,
    takeProfit: Option[BigDecimal] = None
) derives Codec.AsObject:
  def toOrder(position: TradeOrder.Position, cp: CurrencyPair, price: BigDecimal): TradeOrder =
    TradeOrder.Enter(position, cp, price, volume)

final case class TradeSettings(
    strategy: TradeStrategy,
    broker: BrokerParameters,
    trading: TradingParameters
) derives Codec.AsObject

final case class GlobalSettings(
    userId: UserId,
    signal: Option[SignalSettings],
    trade: Option[TradeSettings],
    note: Option[String]
)
