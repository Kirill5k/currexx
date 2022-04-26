package currexx.core.market

import currexx.core.signal.Signal

trait MarketService[F[_]]:
  def processSignal(signal: Signal): F[Unit]
