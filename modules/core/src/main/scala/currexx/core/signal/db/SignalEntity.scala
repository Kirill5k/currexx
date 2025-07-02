package currexx.core.signal.db

import io.circe.Codec
import currexx.domain.user.UserId
import currexx.core.signal.Signal
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.signal.{Condition, Indicator}
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class SignalEntity(
    userId: ObjectId,
    currencyPair: CurrencyPair,
    interval: Interval,
    condition: Condition,
    triggeredBy: Indicator,
    time: Instant
) derives Codec.AsObject:
  def toDomain: Signal = Signal(
    userId = UserId(userId),
    currencyPair = currencyPair,
    interval = interval, 
    condition = condition,
    triggeredBy = triggeredBy,
    time = time
  )

object SignalEntity:
  def from(signal: Signal): SignalEntity =
    SignalEntity(
      userId = signal.userId.toObjectId,
      currencyPair = signal.currencyPair,
      interval = signal.interval,
      condition = signal.condition,
      triggeredBy = signal.triggeredBy, 
      time = signal.time
    )
