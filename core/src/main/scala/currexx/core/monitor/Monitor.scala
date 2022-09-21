package currexx.core.monitor

import currexx.domain.user.UserId
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.Schedule
import currexx.domain.types.IdType
import currexx.core.common.time.*
import io.circe.Codec

import java.time.Instant
import scala.concurrent.duration.{Duration, FiniteDuration}

opaque type MonitorId = String
object MonitorId extends IdType[MonitorId]

trait MonitorSchedule {
  def schedule: Schedule
  def lastQueriedAt: Option[Instant]

  def durationBetweenNextQuery(now: Instant): FiniteDuration =
    lastQueriedAt
      .map(schedule.nextExecutionTime)
      .filter(_.isAfter(now))
      .map(now.durationBetween)
      .getOrElse {
        schedule match
          case _: Schedule.Periodic => Duration.Zero
          case _: Schedule.Cron     => now.durationBetween(schedule.nextExecutionTime(now))
      }
}

final case class PriceMonitorSchedule(
    interval: Interval,
    schedule: Schedule,
    lastQueriedAt: Option[Instant]
) extends MonitorSchedule
    derives Codec.AsObject

final case class ProfitMonitorSchedule(
    min: Option[BigDecimal],
    max: Option[BigDecimal],
    schedule: Schedule,
    lastQueriedAt: Option[Instant]
) extends MonitorSchedule
    derives Codec.AsObject

final case class Monitor(
    id: MonitorId,
    userId: UserId,
    active: Boolean,
    currencyPair: CurrencyPair,
    price: PriceMonitorSchedule,
    profit: Option[ProfitMonitorSchedule]
)

final case class CreateMonitor(
    userId: UserId,
    currencyPair: CurrencyPair,
    price: PriceMonitorSchedule,
    profit: Option[ProfitMonitorSchedule]
)
