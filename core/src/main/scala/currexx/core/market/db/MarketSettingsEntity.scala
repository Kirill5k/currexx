package currexx.core.market.db

import currexx.clients.broker.BrokerParameters
import currexx.core.market.{MarketSettings, TradingParameters}
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

final case class MarketSettingsEntity(
    _id: ObjectId,
    userId: ObjectId,
    broker: BrokerParameters,
    trading: TradingParameters
) derives Codec.AsObject:
  def toDomain: MarketSettings = MarketSettings(UserId(userId), broker, trading)

object MarketSettingsEntity:
  def from(ms: MarketSettings): MarketSettingsEntity =
    MarketSettingsEntity(ObjectId(), ms.userId.toObjectId, ms.broker, ms.trading)
