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

import java.time.Instant
import scala.concurrent.duration.{Duration, FiniteDuration}

trait MonitorService[F[_]]:
  def rescheduleAll: F[Unit]
  def create(cm: CreateMonitor): F[MonitorId]
  def update(mon: Monitor): F[Unit]
  def getAll(uid: UserId): F[List[Monitor]]
  def get(uid: UserId, id: MonitorId): F[Monitor]
  def delete(uid: UserId, id: MonitorId): F[Unit]
  def pause(uid: UserId, id: MonitorId): F[Unit]
  def resume(uid: UserId, id: MonitorId): F[Unit]
  def queryPrice(uid: UserId, id: MonitorId, manual: Boolean = false): F[Unit]

final private class LiveMonitorService[F[_]](
    private val repository: MonitorRepository[F],
    private val actionDispatcher: ActionDispatcher[F]
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
    repository.create(cm).flatTap(schedulePriceMonitor(cm.price, cm.userId))

  override def queryPrice(uid: UserId, id: MonitorId, manual: Boolean = false): F[Unit] =
    for
      mon <- get(uid, id)
      _ <- F.whenA(mon.active) {
        actionDispatcher
          .dispatch(Action.FetchMarketData(uid, mon.currencyPair, mon.price.interval))
          .flatTap(_ => repository.updatePriceQueriedTimestamp(uid, id))
      }
      _ <- F.unlessA(manual)(schedulePriceMonitor(mon.price, uid)(id))
    yield ()

  override def rescheduleAll: F[Unit] =
    F.realTimeInstant.flatMap { now =>
      repository.stream
        .map { mon =>
          val db = mon.price.durationBetweenNextQuery(now)
          actionDispatcher.dispatch(Action.SchedulePriceMonitor(mon.userId, mon.id, db))
        }
        .map(Stream.eval)
        .parJoinUnbounded
        .compile
        .drain
    }

  private def schedulePriceMonitor(ms: MonitorSchedule, uid: UserId)(mid: MonitorId): F[Unit] =
    F.realTimeInstant
      .map(ms.durationBetweenNextQuery)
      .flatMap(db => actionDispatcher.dispatch(Action.SchedulePriceMonitor(uid, mid, db)))
}

object MonitorService:
  def make[F[_]: Temporal](
      repository: MonitorRepository[F],
      actionDispatcher: ActionDispatcher[F]
  ): F[MonitorService[F]] =
    Monad[F].pure(LiveMonitorService(repository, actionDispatcher))
