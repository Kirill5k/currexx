package currexx.backtest.services

import cats.effect.Async
import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId, MonitorService}
import currexx.domain.user.UserId

final private class TestMonitorService[F[_]](using
    F: Async[F]
) extends MonitorService[F]:
  override def rescheduleAll: F[Unit]                                               = F.unit
  override def create(cm: CreateMonitor): F[MonitorId]                              = F.raiseError(new RuntimeException("unimplemented"))
  override def update(mon: Monitor): F[Unit]                                        = F.unit
  override def getAll(uid: UserId): F[List[Monitor]]                                = F.pure(Nil)
  override def get(uid: UserId, id: MonitorId): F[Monitor]                          = F.raiseError(new RuntimeException("unimplemented"))
  override def delete(uid: UserId, id: MonitorId): F[Unit]                          = F.unit
  override def pause(uid: UserId, id: MonitorId): F[Unit]                           = F.unit
  override def resume(uid: UserId, id: MonitorId): F[Unit]                          = F.unit
  override def scheduleProfit(uid: UserId, id: MonitorId, manual: Boolean): F[Unit] = F.unit
  override def schedulePrice(uid: UserId, id: MonitorId, manual: Boolean): F[Unit]  = F.unit

object TestMonitorService:
  def make[F[_]: Async]: F[MonitorService[F]] =
    Async[F].pure(TestMonitorService[F])
