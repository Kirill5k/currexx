package currexx.core.trade.db

import currexx.clients.broker.BrokerParameters
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, PriceRange, TradeOrder}
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class TradeOrderEntity(
    _id: ObjectId,
    userId: ObjectId,
    currencyPair: CurrencyPair,
    order: TradeOrder,
    broker: BrokerParameters,
    currentPrice: PriceRange,
    time: Instant
) derives Codec.AsObject:
  def toDomain: TradeOrderPlacement =
    TradeOrderPlacement(
      UserId(userId),
      currencyPair,
      order,
      broker,
      currentPrice,
      time
    )

object TradeOrderEntity:
  def from(top: TradeOrderPlacement): TradeOrderEntity =
    TradeOrderEntity(ObjectId.gen, top.userId.toObjectId, top.currencyPair, top.order, top.broker, top.currentPrice, top.time)
