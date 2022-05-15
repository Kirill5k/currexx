package currexx.core.fixtures

import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId}
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.Schedule
import currexx.domain.user.UserId
import org.bson.types.ObjectId

import java.time.Instant
import scala.concurrent.duration.*

object Monitors {
  lazy val mid: MonitorId         = MonitorId(ObjectId.get())
  lazy val queriedAt: Instant     = Instant.now
  lazy val period: FiniteDuration = 3.hours
  lazy val schedule: Schedule     = Schedule.Periodic(period)
  lazy val monitor: Monitor       = Monitor(mid, Users.uid, true, Markets.gbpeur, Interval.H1, period, schedule, Some(queriedAt))

  def create(
      uid: UserId = Users.uid,
      pair: CurrencyPair = Markets.gbpeur,
      interval: Interval = Interval.H1,
      period: FiniteDuration = 3.hours,
      schedule: Schedule = schedule
  ): CreateMonitor = CreateMonitor(uid, pair, interval, period, schedule)
}
