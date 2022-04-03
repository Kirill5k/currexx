package currexx.core.signal.db

import io.circe.Codec
import currexx.core.auth.user.UserId
import currexx.core.signal.{CurrencyPair, Indicator, Signal}
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class SignalEntity(
    _id: ObjectId,
    userId: ObjectId,
    currencyPair: CurrencyPair,
    indicator: Indicator,
    time: Instant
) derives Codec.AsObject:
  def toDomain: Signal = Signal(UserId(userId), currencyPair, indicator, time)

object SignalEntity:
  def from(signal: Signal): SignalEntity =
    SignalEntity(ObjectId(), signal.userId.toObjectId, signal.currencyPair, signal.indicator, signal.time)
