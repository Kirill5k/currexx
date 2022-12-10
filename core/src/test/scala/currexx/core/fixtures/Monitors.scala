package currexx.core.fixtures

import cats.data.NonEmptyList
import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId}
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.{Limits, Schedule}
import currexx.domain.user.UserId
import mongo4cats.bson.ObjectId

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

object Monitors {
  lazy val mid: MonitorId     = MonitorId(ObjectId.gen)
  lazy val queriedAt: Instant = Instant.now.truncatedTo(ChronoUnit.MILLIS)

  lazy val limits = Limits(Some(-10), Some(150), None, None, false)

  private val cps     = NonEmptyList.of(Markets.gbpeur)
  lazy val marketData = Monitor.MarketData(mid, Users.uid, true, cps, Schedule.Periodic(3.hours), Some(queriedAt), Interval.H1)
  lazy val profit     = Monitor.Profit(mid, Users.uid, true, cps, Schedule.Periodic(3.hours), Some(queriedAt), limits)

  def createMarketData(
      uid: UserId = Users.uid,
      pairs: NonEmptyList[CurrencyPair] = cps,
      schedule: Schedule = Schedule.Periodic(3.hours),
      interval: Interval = Interval.H1
  ): CreateMonitor = CreateMonitor.MarketData(uid, pairs, schedule, interval)

  def genMarketData(
      mid: MonitorId = MonitorId(ObjectId.gen),
      uid: UserId = Users.uid,
      pairs: NonEmptyList[CurrencyPair] = cps,
      schedule: Schedule = Schedule.Periodic(3.hours),
      interval: Interval = Interval.H1,
      lastQueriedAt: Option[Instant] = Some(queriedAt)
  ): Monitor =
    Monitor.MarketData(mid, uid, true, pairs, schedule, lastQueriedAt, interval)

}
