package currexx.core.signal

import currexx.core.auth.user.UserId

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
  def getAll(uid: UserId): F[List[Signal]]
