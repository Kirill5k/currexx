package currexx.core.trade

import currexx.clients.broker.BrokerParameters
import currexx.domain.market.{OpenedTradeOrder, OrderExecution, TradeOrder}
import currexx.domain.user.UserId

import java.time.Instant

final case class TradeOrderPlacement(
    userId: UserId,
    // The order as decided: its price is the quote the decision was based on, not the fill
    order: TradeOrder,
    broker: BrokerParameters,
    time: Instant,
    executions: List[OrderExecution] = Nil,
    // The broker's position once the order filled; None while it has not
    brokerPosition: Option[BrokerPosition] = None
)

enum BrokerPosition:
  case Flat
  // openPrice is the broker's volume-weighted average over the position's remaining trades
  case Open(position: TradeOrder.Position, openPrice: BigDecimal)
  // The order filled, but the broker's position could not be read
  case Unknown

object BrokerPosition:
  // A missing average price is reported as zero, which must not become a stop anchor
  def from(opened: OpenedTradeOrder): BrokerPosition =
    if opened.openPrice > 0 then Open(opened.position, opened.openPrice) else Unknown
