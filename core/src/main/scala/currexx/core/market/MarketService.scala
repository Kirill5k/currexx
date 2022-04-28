package currexx.core.market

import cats.Monad
import currexx.core.common.action.ActionDispatcher
import currexx.core.signal.Signal
import currexx.core.market.db.MarketSettingsRepository
import currexx.domain.user.UserId

trait MarketService[F[_]]:
  def getSettings(uid: UserId): F[MarketSettings]
  def updateSettings(settings: MarketSettings): F[Unit]
  def processSignal(signal: Signal): F[Unit]

final private class LiveMarketService[F[_]](
    private val settingsRepository: MarketSettingsRepository[F],
    private val dispatcher: ActionDispatcher[F]
) extends MarketService[F]:
  override def getSettings(uid: UserId): F[MarketSettings]       = settingsRepository.get(uid)
  override def updateSettings(settings: MarketSettings): F[Unit] = settingsRepository.update(settings)

  override def processSignal(signal: Signal): F[Unit] = ???

object MarketService:
  def make[F[_]: Monad](
      settingsRepo: MarketSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[MarketService[F]] =
    Monad[F].pure(LiveMarketService[F](settingsRepo, dispatcher))
