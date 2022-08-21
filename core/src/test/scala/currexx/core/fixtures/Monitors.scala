package currexx.core.fixtures

import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId}
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.Schedule
import currexx.domain.user.UserId
import mongo4cats.bson.ObjectId

import java.time.Instant
import scala.concurrent.duration.*

object Monitors {
  lazy val mid: MonitorId     = MonitorId(ObjectId.gen)
  lazy val queriedAt: Instant = Instant.now
  lazy val schedule: Schedule = Schedule.Periodic(3.hours)
  lazy val monitor: Monitor   = Monitor(mid, Users.uid, true, Markets.gbpeur, Interval.H1, schedule, Some(queriedAt))

  def create(
      uid: UserId = Users.uid,
      pair: CurrencyPair = Markets.gbpeur,
      interval: Interval = Interval.H1,
      schedule: Schedule = schedule
  ): CreateMonitor = CreateMonitor(uid, pair, interval, schedule)
}
