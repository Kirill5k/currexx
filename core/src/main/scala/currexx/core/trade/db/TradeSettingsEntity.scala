package currexx.core.trade.db

import currexx.clients.broker.BrokerParameters
import currexx.core.trade.{TradeSettings, TradeStrategy, TradingParameters}
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

final case class TradeSettingsEntity(
    _id: ObjectId,
    userId: ObjectId,
    strategy: TradeStrategy,
    broker: BrokerParameters,
    trading: TradingParameters,
    comment: Option[String]
) derives Codec.AsObject:
  def toDomain: TradeSettings = TradeSettings(UserId(userId), strategy, broker, trading, comment)

object TradeSettingsEntity:
  def from(ts: TradeSettings): TradeSettingsEntity =
    TradeSettingsEntity(ObjectId(), ts.userId.toObjectId, ts.strategy, ts.broker, ts.trading, ts.comment)
