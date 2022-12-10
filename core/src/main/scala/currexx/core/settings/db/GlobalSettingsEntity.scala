package currexx.core.settings.db

import currexx.clients.broker.BrokerParameters
import currexx.core.settings.{GlobalSettings, SignalSettings, TradeSettings}
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

final case class GlobalSettingsEntity(
    _id: ObjectId,
    userId: ObjectId,
    signal: Option[SignalSettings],
    trade: Option[TradeSettings],
    note: Option[String]
) derives Codec.AsObject:
  def toDomain: GlobalSettings = GlobalSettings(UserId(userId), signal, trade, note)

object GlobalSettingsEntity:
  def from(
      uid: UserId,
      signal: Option[SignalSettings] = None,
      trade: Option[TradeSettings] = None,
      note: Option[String] = None
  ): GlobalSettingsEntity = GlobalSettingsEntity(ObjectId.gen, uid.toObjectId, signal, trade, note)
