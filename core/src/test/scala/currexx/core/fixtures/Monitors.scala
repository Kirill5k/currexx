package currexx.core.fixtures

import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId, PriceMonitorSchedule}
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.Schedule
import currexx.domain.user.UserId
import mongo4cats.bson.ObjectId

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

object Monitors {
  lazy val mid: MonitorId                             = MonitorId(ObjectId.gen)
  lazy val queriedAt: Instant                         = Instant.now.truncatedTo(ChronoUnit.MILLIS)
  lazy val priceMonitorSchedule: PriceMonitorSchedule = PriceMonitorSchedule(Interval.H1, Schedule.Periodic(3.hours), Some(queriedAt))
  lazy val monitor: Monitor                           = Monitor(mid, Users.uid, true, Markets.gbpeur, priceMonitorSchedule)

  def create(
      uid: UserId = Users.uid,
      pair: CurrencyPair = Markets.gbpeur,
      price: PriceMonitorSchedule = priceMonitorSchedule.copy(lastQueriedAt = None)
  ): CreateMonitor = CreateMonitor(uid, pair, price)
}
