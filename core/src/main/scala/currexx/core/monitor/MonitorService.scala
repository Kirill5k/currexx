package currexx.core.monitor

import cats.Monad
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.data.MarketDataClient
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.time.*
import currexx.core.monitor.db.MonitorRepository
import currexx.domain.market.CurrencyPair
import currexx.domain.monitor.Schedule
import currexx.domain.user.UserId
import fs2.Stream

import scala.concurrent.duration.Duration

trait MonitorService[F[_]]:
  def rescheduleAll: F[Unit]
  def create(cm: CreateMonitor): F[MonitorId]
  def update(mon: Monitor): F[Unit]
  def getAll(uid: UserId): F[List[Monitor]]
  def get(uid: UserId, id: MonitorId): F[Monitor]
  def delete(uid: UserId, id: MonitorId): F[Unit]
  def pause(uid: UserId, id: MonitorId): F[Unit]
  def resume(uid: UserId, id: MonitorId): F[Unit]
  def query(uid: UserId, id: MonitorId, manual: Boolean = false): F[Unit]

final private class LiveMonitorService[F[_]](
    private val repository: MonitorRepository[F],
    private val actionDispatcher: ActionDispatcher[F],
    private val marketDataClient: MarketDataClient[F]
)(using
    F: Temporal[F]
) extends MonitorService[F] {
  private def closeOpenOrders(m: Monitor): F[Unit]         = actionDispatcher.dispatch(Action.CloseOpenOrders(m.userId, m.currencyPair))
  override def getAll(uid: UserId): F[List[Monitor]]       = repository.getAll(uid)
  override def get(uid: UserId, id: MonitorId): F[Monitor] = repository.find(uid, id)
  override def delete(uid: UserId, id: MonitorId): F[Unit] = repository.delete(uid, id).flatMap(closeOpenOrders)
  override def pause(uid: UserId, id: MonitorId): F[Unit]  = repository.activate(uid, id, false).flatMap(closeOpenOrders)
  override def resume(uid: UserId, id: MonitorId): F[Unit] = repository.activate(uid, id, true).void
  override def update(mon: Monitor): F[Unit] =
    repository.update(mon).flatMap { oldMon =>
      F.whenA(oldMon.currencyPair != mon.currencyPair || (oldMon.active && !mon.active))(closeOpenOrders(oldMon))
    }

  override def create(cm: CreateMonitor): F[MonitorId] =
    repository.create(cm).flatTap(scheduleNew(cm.schedule, cm.userId))

  private def scheduleNew(schedule: Schedule, uid: UserId)(mid: MonitorId): F[Unit] =
    schedule match
      case _: Schedule.Periodic => actionDispatcher.dispatch(Action.QueryMonitor(uid, mid))
      case _: Schedule.Cron     => reschedule(uid, mid, schedule)

  override def query(uid: UserId, id: MonitorId, manual: Boolean = false): F[Unit] =
    for
      mon <- get(uid, id)
      _ <- F.whenA(mon.active) {
        marketDataClient
          .timeSeriesData(mon.currencyPair, mon.interval)
          .flatMap(tsd => actionDispatcher.dispatch(Action.ProcessMarketData(uid, tsd)))
          .flatTap(_ => repository.updateQueriedTimestamp(uid, id))
      }
      _ <- F.whenA(!manual)(reschedule(uid, id, mon.schedule))
    yield ()

  private def reschedule(uid: UserId, mid: MonitorId, schedule: Schedule): F[Unit] =
    for
      now <- F.realTimeInstant
      next = schedule.nextExecutionTime(now)
      _ <- actionDispatcher.dispatch(Action.ScheduleMonitor(uid, mid, now.durationBetween(next)))
    yield ()

  override def rescheduleAll: F[Unit] =
    F.realTimeInstant.flatMap { now =>
      repository.stream
        .map { mon =>
          mon.lastQueriedAt
            .map(mon.schedule.nextExecutionTime)
            .filter(_.isAfter(now))
            .map(now.durationBetween(_)) match
            case Some(db) => actionDispatcher.dispatch(Action.ScheduleMonitor(mon.userId, mon.id, db))
            case None     => scheduleNew(mon.schedule, mon.userId)(mon.id)
        }
        .map(Stream.eval)
        .parJoinUnbounded
        .compile
        .drain
    }
}

object MonitorService:
  def make[F[_]: Temporal](
      repository: MonitorRepository[F],
      actionDispatcher: ActionDispatcher[F],
      marketDataClient: MarketDataClient[F]
  ): F[MonitorService[F]] =
    Monad[F].pure(LiveMonitorService(repository, actionDispatcher, marketDataClient))
