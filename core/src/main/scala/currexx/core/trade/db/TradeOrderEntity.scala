package currexx.core.trade.db

import currexx.clients.broker.BrokerParameters
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.TradeOrder
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class TradeOrderEntity(
    userId: ObjectId,
    order: TradeOrder,
    broker: BrokerParameters,
    time: Instant
) derives Codec.AsObject:
  def toDomain: TradeOrderPlacement =
    TradeOrderPlacement(
      UserId(userId),
      order,
      broker,
      time
    )

object TradeOrderEntity:
  def from(top: TradeOrderPlacement): TradeOrderEntity =
    TradeOrderEntity(top.userId.toObjectId, top.order, top.broker, top.time)
