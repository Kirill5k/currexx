package currexx.core.trade.db

import currexx.clients.broker.BrokerParameters
import currexx.core.trade.{TradeSettings, TradingParameters}
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

final case class TradeSettingsEntity(
    _id: ObjectId,
    userId: ObjectId,
    broker: BrokerParameters,
    trading: TradingParameters
) derives Codec.AsObject:
  def toDomain: TradeSettings = TradeSettings(UserId(userId), broker, trading)

object TradeSettingsEntity:
  def from(ms: TradeSettings): TradeSettingsEntity =
    TradeSettingsEntity(ObjectId(), ms.userId.toObjectId, ms.broker, ms.trading)
