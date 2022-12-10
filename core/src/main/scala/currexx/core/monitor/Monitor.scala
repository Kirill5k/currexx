package currexx.core.monitor

import cats.data.NonEmptyList
import currexx.domain.user.UserId
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.{Limits, Schedule}
import currexx.domain.types.IdType
import currexx.core.common.time.*

import java.time.Instant
import scala.concurrent.duration.{Duration, FiniteDuration}

opaque type MonitorId = String
object MonitorId extends IdType[MonitorId]

sealed trait Monitor(val kind: String):
  def id: MonitorId
  def userId: UserId
  def active: Boolean
  def currencyPairs: NonEmptyList[CurrencyPair]
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

object Monitor:
  final case class MarketData(
      id: MonitorId,
      userId: UserId,
      active: Boolean,
      currencyPairs: NonEmptyList[CurrencyPair],
      schedule: Schedule,
      lastQueriedAt: Option[Instant],
      interval: Interval
  ) extends Monitor("market-data")

  final case class Profit(
      id: MonitorId,
      userId: UserId,
      active: Boolean,
      currencyPairs: NonEmptyList[CurrencyPair],
      schedule: Schedule,
      lastQueriedAt: Option[Instant],
      limits: Limits
  ) extends Monitor("profit")

sealed trait CreateMonitor(val kind: String):
  def userId: UserId
  def currencyPairs: NonEmptyList[CurrencyPair]
  def schedule: Schedule

object CreateMonitor:
  final case class MarketData(
      userId: UserId,
      currencyPairs: NonEmptyList[CurrencyPair],
      schedule: Schedule,
      interval: Interval
  ) extends CreateMonitor("market-data")

  final case class Profit(
      userId: UserId,
      currencyPairs: NonEmptyList[CurrencyPair],
      schedule: Schedule,
      limits: Limits
  ) extends CreateMonitor("profit")
