package currexx.core.monitor

import currexx.domain.user.UserId
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.Schedule
import currexx.domain.types.IdType

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

opaque type MonitorId = String
object MonitorId extends IdType[MonitorId]

final case class Monitor(
    id: MonitorId,
    userId: UserId,
    active: Boolean,
    currencyPair: CurrencyPair,
    interval: Interval,
    period: FiniteDuration,
    schedule: Schedule,
    lastQueriedAt: Option[Instant]
)

final case class CreateMonitor(
    userId: UserId,
    currencyPair: CurrencyPair,
    interval: Interval,
    period: FiniteDuration,
    schedule: Schedule
)
