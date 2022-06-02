package currexx.core.signal.db

import cats.syntax.option.*
import currexx.domain.market.{CurrencyPair, Indicator}
import currexx.domain.user.UserId
import currexx.core.signal.{SignalSettings, TriggerFrequency}
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

final case class SignalSettingsEntity(
    _id: ObjectId,
    userId: ObjectId,
    triggerFrequency: Option[TriggerFrequency],
    indicators: Set[Indicator]
) derives Codec.AsObject:
  def toDomain: SignalSettings =
    SignalSettings(
      UserId(userId),
      triggerFrequency.getOrElse(TriggerFrequency.OncePerDay),
      indicators.toList
    )

object SignalSettingsEntity:
  def from(settings: SignalSettings): SignalSettingsEntity =
    SignalSettingsEntity(
      ObjectId.get,
      settings.userId.toObjectId,
      settings.triggerFrequency.some,
      settings.indicators.toSet
    )
