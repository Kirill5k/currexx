package currexx.core.monitor.db

import io.circe.Codec
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId}
import currexx.domain.user.UserId
import currexx.domain.json.given
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

final case class MonitorEntity(
    _id: ObjectId,
    userId: ObjectId,
    active: Boolean,
    currencyPair: CurrencyPair,
    interval: Interval,
    period: FiniteDuration,
    lastQueriedAt: Option[Instant]
) derives Codec.AsObject:
  def toDomain: Monitor =
    Monitor(
      MonitorId(_id),
      UserId(userId),
      active,
      currencyPair,
      interval,
      period,
      lastQueriedAt
    )

object MonitorEntity {
  def from(create: CreateMonitor): MonitorEntity =
    MonitorEntity(
      ObjectId.get,
      create.userId.toObjectId,
      true,
      create.currencyPair,
      create.interval,
      create.period,
      None
    )
}
