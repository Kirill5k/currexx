package currexx.core.trade.db

import currexx.clients.broker.BrokerParameters
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{OrderExecution, TradeOrder}
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class TradeOrderEntity(
    userId: ObjectId,
    order: TradeOrder,
    broker: BrokerParameters,
    time: Instant,
    // Optional so that orders stored before executions were recorded still decode
    executions: Option[List[OrderExecution]]
) derives Codec.AsObject:
  def toDomain: TradeOrderPlacement =
    TradeOrderPlacement(
      UserId(userId),
      order,
      broker,
      time,
      executions.getOrElse(Nil)
    )

object TradeOrderEntity:
  def from(top: TradeOrderPlacement): TradeOrderEntity =
    TradeOrderEntity(top.userId.toObjectId, top.order, top.broker, top.time, Some(top.executions))
