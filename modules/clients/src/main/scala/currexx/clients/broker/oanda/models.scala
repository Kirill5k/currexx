package currexx.clients.broker.oanda

import currexx.clients.broker.oanda.OandaBrokerClient.LotSize
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, OrderExecution, OrderPlacementResult, OrderRef, TradeOrder}
import io.circe.{Codec, JsonObject}

import java.time.Instant

final private[oanda] case class ClosePositionRequest(
    longUnits: String,
    shortUnits: String
) derives Codec.AsObject

final private[oanda] case class OpenPositionRequest(order: OpenPositionOrder) derives Codec.AsObject

// Close outcomes depend on fill presence and cancellation reasons, not transaction metadata.
final private[oanda] case class ClosePositionResponse(
    lastTransactionID: String,
    longOrderFillTransaction: Option[JsonObject],
    longOrderCancelTransaction: Option[CloseOrderCancelTransaction],
    shortOrderFillTransaction: Option[JsonObject],
    shortOrderCancelTransaction: Option[CloseOrderCancelTransaction]
) derives Codec.AsObject:
  def fills: List[JsonObject]                          = List(longOrderFillTransaction, shortOrderFillTransaction).flatten
  def cancellations: List[CloseOrderCancelTransaction] = List(longOrderCancelTransaction, shortOrderCancelTransaction).flatten
  def isMarketHalted: Boolean                          =
    fills.isEmpty && cancellations.nonEmpty && cancellations.forall(_.reason == "MARKET_HALTED")

  def missingFills(request: ClosePositionRequest): List[String] = List(
    Option.when(request.longUnits == "ALL" && longOrderFillTransaction.isEmpty)("long"),
    Option.when(request.shortUnits == "ALL" && shortOrderFillTransaction.isEmpty)("short")
  ).flatten

final private[oanda] case class CloseOrderCancelTransaction(reason: String) derives Codec.AsObject

final private[oanda] case class OpenPositionResponse(
    orderCreateTransaction: OrderTransaction,
    orderFillTransaction: Option[OrderFillTransaction],
    orderCancelTransaction: Option[OrderCancelTransaction],
    relatedTransactionIDs: List[String],
    lastTransactionID: String
) derives Codec.AsObject:
  def isMarketHalted: Boolean =
    orderFillTransaction.isEmpty && orderCancelTransaction.exists(_.reason == "MARKET_HALTED")

  def toResult(clientOrderId: String): OrderPlacementResult =
    (orderCancelTransaction, orderFillTransaction) match
      case (Some(cancel), _)  => OrderPlacementResult.Cancelled(cancel.reason)
      case (None, Some(fill)) => OrderPlacementResult.filled(fill.toExecution)
      case (None, None)       => OrderPlacementResult.Pending(OrderRef(clientOrderId, Some(orderCreateTransaction.id)))

final private[oanda] case class OrderTransaction(
    id: String,
    time: Instant,
    userID: Int,
    accountID: String,
    batchID: String,
    requestID: Option[String]
) derives Codec.AsObject

// Only the fields that make up an execution are decoded, so unrelated metadata cannot break fill detection.
final private[oanda] case class OrderFillTransaction(
    id: String,
    orderID: String,
    time: Instant,
    units: BigDecimal,
    price: BigDecimal,
    fullVWAP: Option[BigDecimal],
    tradeOpened: Option[TradeRef],
    tradesClosed: Option[List[TradeRef]],
    tradeReduced: Option[TradeRef]
) derives Codec.AsObject:
  def toExecution: OrderExecution =
    OrderExecution(
      price = fullVWAP.getOrElse(price),
      time = time,
      volume = units.abs / LotSize,
      orderId = orderID,
      transactionId = id,
      tradeIds = tradeOpened.toList.map(_.tradeID) ++ tradeReduced.map(_.tradeID) ++ tradesClosed.toList.flatten.map(_.tradeID)
    )

final private[oanda] case class TradeRef(tradeID: String) derives Codec.AsObject

final private[oanda] case class FillTransactionResponse(transaction: OrderFillTransaction) derives Codec.AsObject

final private[oanda] case class OrderCancelTransaction(
    id: String,
    time: Instant,
    userID: Int,
    accountID: String,
    batchID: String,
    requestID: Option[String],
    `type`: String,
    reason: String
) derives Codec.AsObject

private[oanda] object OpenPositionRequest:
  def from(order: TradeOrder.Enter, clientOrderId: String): OpenPositionRequest =
    val units = order.position match
      case TradeOrder.Position.Buy  => (order.volume * LotSize).toInt
      case TradeOrder.Position.Sell => -(order.volume * LotSize).toInt
    OpenPositionRequest(
      OpenPositionOrder(
        instrument = s"${order.currencyPair.base}_${order.currencyPair.quote}",
        units = units,
        `type` = "MARKET",
        positionFill = "DEFAULT",
        clientExtensions = ClientExtensions(clientOrderId)
      )
    )

final private[oanda] case class OpenPositionOrder(
    instrument: String,
    units: Int,
    `type`: String,
    positionFill: String,
    clientExtensions: ClientExtensions
) derives Codec.AsObject

final private[oanda] case class ClientExtensions(id: String) derives Codec.AsObject

final private[oanda] case class OrderResponse(order: Order) derives Codec.AsObject

final private[oanda] case class Order(id: String, state: String, fillingTransactionID: Option[String]) derives Codec.AsObject

final private[oanda] case class AccountsResponse(accounts: List[Account]) derives Codec.AsObject

final private[oanda] case class Account(id: String) derives Codec.AsObject

final private[oanda] case class PositionsResponse(positions: List[Position]) derives Codec.AsObject

final private[oanda] case class PositionResponse(position: Position) derives Codec.AsObject

final private[oanda] case class Position(
    instrument: String,
    long: PositionSide,
    short: PositionSide
) derives Codec.AsObject {
  def isOpen: Boolean   = long.units != 0 || short.units != 0
  def isHedged: Boolean = long.units != 0 && short.units != 0
  // A delayed close must not close a replacement position opened while the original request was waiting.
  def hasSameTradesAs(other: Position): Boolean =
    def sameSide(left: PositionSide, right: PositionSide): Boolean =
      if left.units == 0 then right.units == 0
      else
        left.units.signum == right.units.signum &&
        left.tradeIDs.exists(ids => ids.nonEmpty && right.tradeIDs.exists(_.toSet == ids.toSet))
    sameSide(long, other.long) && sameSide(short, other.short)

  def side(position: TradeOrder.Position): PositionSide = position match
    case TradeOrder.Position.Buy  => long
    case TradeOrder.Position.Sell => short
  def toClosePositionRequest: ClosePositionRequest =
    ClosePositionRequest(
      longUnits = if (long.units == 0) "NONE" else "ALL",
      shortUnits = if (short.units == 0) "NONE" else "ALL"
    )
  def toOpenedTradeOrder: Option[OpenedTradeOrder] =
    Option.when(isOpen) {
      val isBuy = long.units > 0
      val side  = if isBuy then long else short
      OpenedTradeOrder(
        currencyPair = CurrencyPair.fromUnsafe(instrument.replace("_", "")),
        position = if isBuy then TradeOrder.Position.Buy else TradeOrder.Position.Sell,
        openPrice = side.averagePrice.getOrElse(BigDecimal(0)),
        currentPrice = side.averagePrice.getOrElse(BigDecimal(0)) +
          Option.when(side.units != 0)(side.unrealizedPL / side.units).getOrElse(BigDecimal(0)),
        volume = side.units.abs / LotSize,
        profit = side.unrealizedPL
      )
    }
}

final private[oanda] case class PositionSide(
    units: BigDecimal,
    tradeIDs: Option[List[String]],
    averagePrice: Option[BigDecimal],
    unrealizedPL: BigDecimal
) derives Codec.AsObject
