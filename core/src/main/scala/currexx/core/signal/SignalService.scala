package currexx.core.signal

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
