package currexx.core.trade

import cats.Monad
import cats.effect.kernel.Temporal
import cats.syntax.apply.*
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.action.ActionDispatcher
import currexx.core.market.MarketState
import currexx.core.trade.TradeStrategyExecutor.Outcome
import currexx.core.trade.db.TradeSettingsRepository
import currexx.domain.market.{CurrencyPair, Indicator, TradeOrder}
import currexx.domain.user.UserId

trait TradeService[F[_]]:
  def getSettings(uid: UserId): F[TradeSettings]
  def updateSettings(settings: TradeSettings): F[Unit]
  def processMarketState(state: MarketState, trigger: Indicator): F[Unit]

final private class LiveTradeService[F[_]](
    private val settingsRepository: TradeSettingsRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F]
) extends TradeService[F] {
  override def getSettings(uid: UserId): F[TradeSettings]       = settingsRepository.get(uid)
  override def updateSettings(settings: TradeSettings): F[Unit] = settingsRepository.update(settings)

  override def processMarketState(state: MarketState, trigger: Indicator): F[Unit] =
    (settingsRepository.get(state.userId), F.realTimeInstant)
      .mapN { (sett, time) =>
        val strategyResult = TradeStrategyExecutor.get(sett.strategy).analyze(state, trigger)
        (state.latestPrice, strategyResult).mapN { (price, result) =>
          val order = result match
            case Outcome.Buy   => buy(state.currencyPair, sett.trading)
            case Outcome.Sell  => sell(state.currencyPair, sett.trading)
            case Outcome.Close => TradeOrder.Exit(state.currencyPair)
          TradeOrderPlacement(state.userId, order, price, time)
        }
      }
      .flatMap { //TODO: send request to broker, save order in repo, emit event to update state
        case Some(order) => ().pure[F]
        case None        => ().pure[F]
      }

  private def buy(pair: CurrencyPair, params: TradingParameters): TradeOrder =
    TradeOrder.Enter(pair, TradeOrder.Position.Buy, params.volume, params.stopLoss, params.trailingStopLoss, params.takeProfit)

  private def sell(pair: CurrencyPair, params: TradingParameters): TradeOrder =
    TradeOrder.Enter(pair, TradeOrder.Position.Sell, params.volume, params.stopLoss, params.trailingStopLoss, params.takeProfit)
}

object TradeService:
  def make[F[_]: Temporal](
      settingsRepo: TradeSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    Monad[F].pure(LiveTradeService[F](settingsRepo, dispatcher))
