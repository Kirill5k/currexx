package currexx.core.market

import currexx.core.signal.Signal
import currexx.domain.user.UserId

trait MarketService[F[_]]:
  def getSettings(uid: UserId): F[Option[MarketSettings]]
  def updateSettings(settings: MarketSettings): F[Unit]
  def processSignal(signal: Signal): F[Unit]
