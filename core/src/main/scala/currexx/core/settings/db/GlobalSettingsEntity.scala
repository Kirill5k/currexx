package currexx.core.settings.db

import currexx.clients.broker.BrokerParameters
import currexx.core.settings.{GlobalSettings, SignalParameters, TradeParameters}
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

final case class GlobalSettingsEntity(
    _id: ObjectId,
    userId: ObjectId,
    signal: Option[SignalParameters],
    trade: Option[TradeParameters]
) derives Codec.AsObject:
  def toDomain: GlobalSettings = GlobalSettings(UserId(userId), signal, trade)

object GlobalSettingsEntity:
  def from(
      uid: UserId,
      signal: Option[SignalParameters] = None,
      trade: Option[TradeParameters] = None
  ): GlobalSettingsEntity = GlobalSettingsEntity(ObjectId.gen, uid.toObjectId, signal, trade)
