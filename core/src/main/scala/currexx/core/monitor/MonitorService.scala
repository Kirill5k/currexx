package currexx.core.monitor

import cats.Monad
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.data.MarketDataClient
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.time.*
import currexx.core.monitor.db.MonitorRepository
import currexx.domain.errors.AppError
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
  def triggerPriceMonitor(uid: UserId, id: MonitorId, manual: Boolean = false): F[Unit]
  def triggerProfitMonitor(uid: UserId, id: MonitorId, manual: Boolean = false): F[Unit]

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
    for
      old <- repository.update(mon)
      _   <- F.whenA(old.currencyPair != mon.currencyPair || (old.active && !mon.active))(closeOpenOrders(old))
      _ <- F.whenA(old.profit.isEmpty && mon.profit.isDefined)(scheduleProfitMonitor(mon.id, mon.userId, mon.profit.get, F.realTimeInstant))
    yield ()

  override def create(cm: CreateMonitor): F[MonitorId] =
    for
      mid <- repository.create(cm)
      _   <- schedulePriceMonitor(mid, cm.userId, cm.price, F.realTimeInstant)
      _   <- F.whenA(cm.profit.isDefined)(scheduleProfitMonitor(mid, cm.userId, cm.profit.get, F.realTimeInstant))
    yield mid

  override def triggerPriceMonitor(uid: UserId, id: MonitorId, manual: Boolean = false): F[Unit] =
    for
      mon <- get(uid, id)
      _ <- F.whenA(mon.active) {
        repository.updatePriceQueriedTimestamp(uid, id) >>
          actionDispatcher.dispatch(Action.FetchMarketData(uid, mon.currencyPair, mon.price.interval))
      }
      _ <- F.unlessA(manual)(schedulePriceMonitor(id, uid, mon.price, F.realTimeInstant))
    yield ()

  override def triggerProfitMonitor(uid: UserId, id: MonitorId, manual: Boolean = false): F[Unit] =
    for
      mon    <- get(uid, id)
      profit <- F.fromOption(mon.profit, AppError.NotScheduled("profit"))
      _ <- F.whenA(mon.active) {
        repository.updateProfitQueriedTimestamp(uid, id) >>
          actionDispatcher.dispatch(Action.AssertProfit(uid, mon.currencyPair, profit.min, profit.max))
      }
      _ <- F.unlessA(manual)(scheduleProfitMonitor(id, uid, profit, F.realTimeInstant))
    yield ()

  override def rescheduleAll: F[Unit] =
    F.realTimeInstant.flatMap { now =>
      repository.stream
        .map { mon =>
          schedulePriceMonitor(mon.id, mon.userId, mon.price, F.pure(now)) >>
            F.whenA(mon.profit.isDefined)(scheduleProfitMonitor(mon.id, mon.userId, mon.profit.get, F.pure(now)))
        }
        .map(Stream.eval)
        .parJoinUnbounded
        .compile
        .drain
    }

  private def schedulePriceMonitor(mid: MonitorId, uid: UserId, ms: MonitorSchedule, now: => F[Instant]): F[Unit] =
    now
      .map(ms.durationBetweenNextQuery)
      .flatMap(db => actionDispatcher.dispatch(Action.SchedulePriceMonitor(uid, mid, db)))

  private def scheduleProfitMonitor(mid: MonitorId, uid: UserId, ms: MonitorSchedule, now: => F[Instant]): F[Unit] =
    now
      .map(ms.durationBetweenNextQuery)
      .flatMap(db => actionDispatcher.dispatch(Action.ScheduleProfitMonitor(uid, mid, db)))
}

object MonitorService:
  def make[F[_]: Temporal](
      repository: MonitorRepository[F],
      actionDispatcher: ActionDispatcher[F]
  ): F[MonitorService[F]] =
    Monad[F].pure(LiveMonitorService(repository, actionDispatcher))
