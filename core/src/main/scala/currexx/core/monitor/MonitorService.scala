package currexx.core.monitor

import cats.Monad
import cats.syntax.flatMap.*
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

final private class LiveMonitorService[F[_]: Monad](
    private val repository: MonitorRepository[F],
    private val actionDispatcher: ActionDispatcher[F]
) extends MonitorService[F] {
  override def getAll(uid: UserId): F[List[Monitor]]       = repository.getAll(uid)
  override def get(uid: UserId, id: MonitorId): F[Monitor] = repository.find(uid, id)
  override def delete(uid: UserId, id: MonitorId): F[Unit] = repository.delete(uid, id)
  override def pause(uid: UserId, id: MonitorId): F[Unit]  = repository.activate(uid, id, false)
  override def resume(uid: UserId, id: MonitorId): F[Unit] = repository.activate(uid, id, true)
  override def create(cm: CreateMonitor): F[MonitorId] =
    repository.create(cm).flatTap(mid => actionDispatcher.dispatch(Action.QueryMonitor(cm.userId, mid)))
}
