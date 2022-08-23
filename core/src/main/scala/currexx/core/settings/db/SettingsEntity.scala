package currexx.core.settings.db

import currexx.clients.broker.BrokerParameters
import currexx.core.settings.{Settings, SignalParameters, TradeParameters}
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

final case class SettingsEntity(
    _id: ObjectId,
    userId: ObjectId,
    signal: Option[SignalParameters],
    trade: Option[TradeParameters]
) derives Codec.AsObject:
  def toDomain: Settings = Settings(UserId(userId), signal, trade)
