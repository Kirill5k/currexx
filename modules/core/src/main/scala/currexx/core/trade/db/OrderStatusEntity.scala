package currexx.core.trade.db

import currexx.clients.broker.Broker
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, OrderPlacementStatus, TradeOrder}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider

import java.time.Instant

final case class OrderStatusEntity(
    userId: ObjectId,
    currencyPair: CurrencyPair,
    orderKind: String,
    position: Option[TradeOrder.Position],
    volume: Option[BigDecimal],
    price: BigDecimal,
    broker: Broker,
    status: OrderPlacementStatus,
    time: Instant
)

object OrderStatusEntity extends MongoJsonCodecs:
  given Codec[OrderStatusEntity]              = deriveCodec[OrderStatusEntity]
  given MongoCodecProvider[OrderStatusEntity] = deriveCirceCodecProvider[OrderStatusEntity]

  def from(top: TradeOrderPlacement, status: OrderPlacementStatus): OrderStatusEntity =
    val (position, volume) = top.order match
      case enter: TradeOrder.Enter => (Some(enter.position), Some(enter.volume))
      case _: TradeOrder.Exit      => (None, None)

    OrderStatusEntity(
      userId = top.userId.toObjectId,
      currencyPair = top.order.currencyPair,
      orderKind = top.order.kind,
      position = position,
      volume = volume,
      price = top.order.price,
      broker = top.broker.broker,
      status = status,
      time = top.time
    )
