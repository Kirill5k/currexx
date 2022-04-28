package currexx.core.market

import cats.Monad
import currexx.core.common.action.ActionDispatcher
import currexx.core.signal.Signal
import currexx.core.market.db.{MarketSettingsRepository, MarketStateRepository}
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.user.UserId

trait MarketService[F[_]]:
  def getSettings(uid: UserId): F[MarketSettings]
  def updateSettings(settings: MarketSettings): F[Unit]
  def getState(uid: UserId): F[List[MarketState]]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit]
  def processSignal(signal: Signal): F[Unit]

final private class LiveMarketService[F[_]](
    private val settingsRepository: MarketSettingsRepository[F],
    private val stateRepo: MarketStateRepository[F],
    private val dispatcher: ActionDispatcher[F]
) extends MarketService[F]:
  override def getSettings(uid: UserId): F[MarketSettings]       = settingsRepository.get(uid)
  override def updateSettings(settings: MarketSettings): F[Unit] = settingsRepository.update(settings)
  override def getState(uid: UserId): F[List[MarketState]]       = stateRepo.getAll(uid)

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] = 
    stateRepo.update(uid, data.currencyPair, data.prices.head)
    
  override def processSignal(signal: Signal): F[Unit] = ???

object MarketService:
  def make[F[_]: Monad](
      settingsRepo: MarketSettingsRepository[F],
      stateRepo: MarketStateRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[MarketService[F]] =
    Monad[F].pure(LiveMarketService[F](settingsRepo, stateRepo, dispatcher))
