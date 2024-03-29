package currexx.core.signal.db

import io.circe.Codec
import currexx.domain.user.UserId
import currexx.core.signal.Signal
import currexx.domain.market.{Condition, CurrencyPair, Indicator}
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class SignalEntity(
    userId: ObjectId,
    currencyPair: CurrencyPair,
    condition: Condition,
    triggeredBy: Indicator,
    time: Instant
) derives Codec.AsObject:
  def toDomain: Signal = Signal(UserId(userId), currencyPair, condition, triggeredBy, time)

object SignalEntity:
  def from(signal: Signal): SignalEntity =
    SignalEntity(signal.userId.toObjectId, signal.currencyPair, signal.condition, signal.triggeredBy, signal.time)
