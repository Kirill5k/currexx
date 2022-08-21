package currexx.core.monitor.db

import io.circe.Codec
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId}
import currexx.domain.user.UserId
import currexx.domain.json.given
import currexx.domain.monitor.Schedule
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
    schedule: Schedule,
    lastQueriedAt: Option[Instant]
) derives Codec.AsObject:
  def toDomain: Monitor =
    Monitor(
      MonitorId(_id),
      UserId(userId),
      active,
      currencyPair,
      interval,
      schedule,
      lastQueriedAt
    )

object MonitorEntity {
  def from(create: CreateMonitor): MonitorEntity =
    MonitorEntity(
      ObjectId.gen,
      create.userId.toObjectId,
      true,
      create.currencyPair,
      create.interval,
      create.schedule,
      None
    )
}
