package currexx.core.trade

import cats.Monad
import currexx.core.common.action.ActionDispatcher
import currexx.core.trade.db.TradeSettingsRepository
import currexx.domain.user.UserId

trait TradeService[F[_]]:
  def getSettings(uid: UserId): F[TradeSettings]
  def updateSettings(settings: TradeSettings): F[Unit]

final private class LiveTradeService[F[_]](
    private val settingsRepository: TradeSettingsRepository[F],
    private val dispatcher: ActionDispatcher[F]
) extends TradeService[F] {
  override def getSettings(uid: UserId): F[TradeSettings]       = settingsRepository.get(uid)
  override def updateSettings(settings: TradeSettings): F[Unit] = settingsRepository.update(settings)
}

object TradeService:
  def make[F[_]: Monad](
      settingsRepo: TradeSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    Monad[F].pure(LiveTradeService[F](settingsRepo, dispatcher))
