package currexx.core.monitor

trait MonitorService[F[_]]:
  def create(monitor: CreateMonitor): F[MonitorId]
