package currexx.core.monitor

import cats.Monad
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.monitor.db.MonitorRepository
import currexx.domain.time.Clock
import currexx.domain.user.UserId
import fs2.Stream

import java.time.Instant

trait MonitorService[F[_]]:
  def rescheduleAll: F[Unit]
  def create(cm: CreateMonitor): F[MonitorId]
  def update(mon: Monitor): F[Unit]
  def getAll(uid: UserId): F[List[Monitor]]
  def get(uid: UserId, id: MonitorId): F[Monitor]
  def delete(uid: UserId, id: MonitorId): F[Unit]
  def pause(uid: UserId, id: MonitorId): F[Unit]
  def resume(uid: UserId, id: MonitorId): F[Unit]
  def triggerMonitor(uid: UserId, id: MonitorId, manual: Boolean = false): F[Unit]

final private class LiveMonitorService[F[_]](
    private val repository: MonitorRepository[F],
    private val actionDispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F],
    clock: Clock[F]
) extends MonitorService[F] {
  override def getAll(uid: UserId): F[List[Monitor]]       = repository.getAll(uid)
  override def get(uid: UserId, id: MonitorId): F[Monitor] = repository.find(uid, id)
  override def delete(uid: UserId, id: MonitorId): F[Unit] = repository.delete(uid, id).void
  override def pause(uid: UserId, id: MonitorId): F[Unit]  = repository.activate(uid, id, false).void
  override def resume(uid: UserId, id: MonitorId): F[Unit] = repository.activate(uid, id, true).void

  override def update(mon: Monitor): F[Unit]           = repository.update(mon).void
  override def create(cm: CreateMonitor): F[MonitorId] = repository.create(cm).flatTap(scheduleMonitor(clock.now)).map(_.id)

  override def triggerMonitor(uid: UserId, id: MonitorId, manual: Boolean = false): F[Unit] =
    for
      mon <- get(uid, id)
      _ <- F.whenA(mon.active) {
        repository.updateQueriedTimestamp(uid, id) >> (mon match
          case md: Monitor.MarketData => actionDispatcher.dispatch(Action.FetchMarketData(uid, mon.currencyPairs, md.interval))
          case p: Monitor.Profit      => actionDispatcher.dispatch(Action.AssertProfit(uid, mon.currencyPairs, p.limits))
        )
      }
      _ <- F.unlessA(manual)(scheduleMonitor(clock.now)(mon))
    yield ()

  override def rescheduleAll: F[Unit] =
    clock.now.flatMap { now =>
      repository.stream
        .map(scheduleMonitor(F.pure(now)))
        .map(Stream.eval)
        .parJoinUnbounded
        .compile
        .drain
    }

  private def scheduleMonitor(now: => F[Instant])(mon: Monitor): F[Unit] =
    now
      .map(mon.durationBetweenNextQuery)
      .flatMap(db => actionDispatcher.dispatch(Action.ScheduleMonitor(mon.userId, mon.id, db)))
}

object MonitorService:
  def make[F[_]: Temporal: Clock](
      repository: MonitorRepository[F],
      actionDispatcher: ActionDispatcher[F]
  ): F[MonitorService[F]] =
    Monad[F].pure(LiveMonitorService(repository, actionDispatcher))
