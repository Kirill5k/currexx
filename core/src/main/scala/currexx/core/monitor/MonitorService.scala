package currexx.core.monitor

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.MarketDataClient
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.monitor.db.MonitorRepository
import currexx.domain.user.UserId

trait MonitorService[F[_]]:
  def create(cm: CreateMonitor): F[MonitorId]
  def getAll(uid: UserId): F[List[Monitor]]
  def get(uid: UserId, id: MonitorId): F[Monitor]
  def delete(uid: UserId, id: MonitorId): F[Unit]
  def pause(uid: UserId, id: MonitorId): F[Unit]
  def resume(uid: UserId, id: MonitorId): F[Unit]
  def query(uid: UserId, id: MonitorId): F[Unit]

final private class LiveMonitorService[F[_]: Monad](
    private val repository: MonitorRepository[F],
    private val actionDispatcher: ActionDispatcher[F],
    private val marketDataClient: MarketDataClient[F]
) extends MonitorService[F] {
  override def getAll(uid: UserId): F[List[Monitor]]       = repository.getAll(uid)
  override def get(uid: UserId, id: MonitorId): F[Monitor] = repository.find(uid, id)
  override def delete(uid: UserId, id: MonitorId): F[Unit] = repository.delete(uid, id)
  override def pause(uid: UserId, id: MonitorId): F[Unit]  = repository.activate(uid, id, false)
  override def resume(uid: UserId, id: MonitorId): F[Unit] = repository.activate(uid, id, true)

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
        else ().pure[F]
      _ <- actionDispatcher.dispatch(Action.ScheduleMonitor(uid, id, mon.period))
    yield ()
}

object MonitorService:
  def make[F[_]: Monad](
      repository: MonitorRepository[F],
      actionDispatcher: ActionDispatcher[F],
      marketDataClient: MarketDataClient[F]
  ): F[MonitorService[F]] =
    Monad[F].pure(LiveMonitorService(repository, actionDispatcher, marketDataClient))
