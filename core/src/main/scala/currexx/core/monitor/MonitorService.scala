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
import currexx.domain.user.UserId

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
  def query(uid: UserId, id: MonitorId): F[Unit]

final private class LiveMonitorService[F[_]](
    private val repository: MonitorRepository[F],
    private val actionDispatcher: ActionDispatcher[F],
    private val marketDataClient: MarketDataClient[F]
)(using
    F: Temporal[F]
) extends MonitorService[F] {
  private def closeOpenOrdersAction(m: Monitor): F[Unit]   = actionDispatcher.dispatch(Action.CloseOpenOrders(m.userId, m.currencyPair))
  override def getAll(uid: UserId): F[List[Monitor]]       = repository.getAll(uid)
  override def get(uid: UserId, id: MonitorId): F[Monitor] = repository.find(uid, id)
  override def delete(uid: UserId, id: MonitorId): F[Unit] = repository.delete(uid, id).flatMap(closeOpenOrdersAction)
  override def pause(uid: UserId, id: MonitorId): F[Unit]  = repository.activate(uid, id, false).flatMap(closeOpenOrdersAction)
  override def resume(uid: UserId, id: MonitorId): F[Unit] = repository.activate(uid, id, true).void
  override def update(mon: Monitor): F[Unit] =
    repository.update(mon).flatMap { oldMon =>
      val deactivatedOrNewCurrency = oldMon.currencyPair != mon.currencyPair || (oldMon.active && !mon.active)
      F.whenA(deactivatedOrNewCurrency)(closeOpenOrdersAction(oldMon))
    }

  override def create(cm: CreateMonitor): F[MonitorId] =
    repository.create(cm).flatTap(mid => actionDispatcher.dispatch(Action.QueryMonitor(cm.userId, mid)))

  override def query(uid: UserId, id: MonitorId): F[Unit] =
    for
      mon <- get(uid, id)
      _ <-
        if (mon.active)
          marketDataClient
            .timeSeriesData(mon.currencyPair, mon.interval)
            .flatMap(tsd => actionDispatcher.dispatch(Action.ProcessMarketData(uid, tsd)))
            .flatTap(_ => repository.updateQueriedTimestamp(uid, id))
        else F.unit
      _ <- actionDispatcher.dispatch(Action.ScheduleMonitor(uid, id, mon.period))
    yield ()

  override def rescheduleAll: F[Unit] =
    F.realTimeInstant.flatMap { now =>
      repository.stream
        .mapAsync(Int.MaxValue) { mon =>
          mon.lastQueriedAt
            .map(now.durationBetween)
            .filter(_ <= mon.period)
            .map(db => actionDispatcher.dispatch(Action.ScheduleMonitor(mon.userId, mon.id, mon.period - db)))
            .getOrElse(actionDispatcher.dispatch(Action.QueryMonitor(mon.userId, mon.id)))
        }
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
