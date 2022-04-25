package currexx.core.signal.db

import currexx.domain.market.{IndicatorParameters, CurrencyPair}
import currexx.domain.user.UserId
import currexx.core.signal.SignalSettings
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

final case class SignalSettingsEntity(
    _id: ObjectId,
    userId: ObjectId,
    indicators: Set[IndicatorParameters]
) derives Codec.AsObject:
  def toDomain: SignalSettings = SignalSettings(UserId(userId), indicators.toList)

object SignalSettingsEntity:
  def from(settings: SignalSettings): SignalSettingsEntity =
    SignalSettingsEntity(
      ObjectId.get,
      settings.userId.toObjectId,
      settings.indicators.toSet
    )
