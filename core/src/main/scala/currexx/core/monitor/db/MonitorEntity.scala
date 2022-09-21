package currexx.core.monitor.db

import io.circe.Codec
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId, PriceMonitorSchedule, ProfitMonitorSchedule}
import currexx.domain.user.UserId
import currexx.domain.json.given
import currexx.domain.monitor.Schedule
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

final case class ProfitMonitor(
    min: Option[BigDecimal],
    max: Option[BigDecimal],
    schedule: Schedule,
    lastQueriedAt: Option[Instant]
) derives Codec.AsObject:
  def toDomain: ProfitMonitorSchedule = ProfitMonitorSchedule(min, max, schedule, lastQueriedAt)

object ProfitMonitor:
  def from(pms: ProfitMonitorSchedule): ProfitMonitor = ProfitMonitor(pms.min, pms.max, pms.schedule, pms.lastQueriedAt)

final case class PriceMonitor(
    interval: Interval,
    schedule: Schedule,
    lastQueriedAt: Option[Instant]
) derives Codec.AsObject:
  def toDomain: PriceMonitorSchedule = PriceMonitorSchedule(interval, schedule, lastQueriedAt)

object PriceMonitor:
  def from(pms: PriceMonitorSchedule): PriceMonitor = PriceMonitor(pms.interval, pms.schedule, pms.lastQueriedAt)

final case class MonitorEntity(
    _id: ObjectId,
    userId: ObjectId,
    active: Boolean,
    currencyPair: CurrencyPair,
    price: PriceMonitor,
    profit: Option[ProfitMonitor]
) derives Codec.AsObject:
  def toDomain: Monitor =
    Monitor(
      MonitorId(_id),
      UserId(userId),
      active,
      currencyPair,
      price.toDomain,
      profit.map(_.toDomain)
    )

object MonitorEntity {
  def from(create: CreateMonitor): MonitorEntity =
    MonitorEntity(
      ObjectId.gen,
      create.userId.toObjectId,
      true,
      create.currencyPair,
      PriceMonitor.from(create.price),
      create.profit.map(ProfitMonitor.from)
    )
}
