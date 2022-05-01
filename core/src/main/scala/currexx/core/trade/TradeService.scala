package currexx.core.trade

import cats.Monad
import cats.effect.kernel.Temporal
import cats.syntax.apply.*
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.clients.broker.BrokerClient
import currexx.core.common.action.ActionDispatcher
import currexx.core.market.MarketState
import currexx.core.trade.TradeStrategyExecutor.Outcome
import currexx.core.trade.db.{TradeOrderRepository, TradeSettingsRepository}
import currexx.domain.market.{CurrencyPair, Indicator, TradeOrder}
import currexx.domain.user.UserId

trait TradeService[F[_]]:
  def getSettings(uid: UserId): F[TradeSettings]
  def updateSettings(settings: TradeSettings): F[Unit]
  def getAllOrders(uid: UserId): F[List[TradeOrderPlacement]]
  def processMarketState(state: MarketState, trigger: Indicator): F[Unit]

final private class LiveTradeService[F[_]](
    private val settingsRepository: TradeSettingsRepository[F],
    private val orderRepository: TradeOrderRepository[F],
    private val brokerClient: BrokerClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F]
) extends TradeService[F] {
  override def getSettings(uid: UserId): F[TradeSettings]              = settingsRepository.get(uid)
  override def updateSettings(settings: TradeSettings): F[Unit]        = settingsRepository.update(settings)
  override def getAllOrders(uid: UserId): F[List[TradeOrderPlacement]] = orderRepository.getAll(uid)

  override def processMarketState(state: MarketState, trigger: Indicator): F[Unit] =
    (settingsRepository.get(state.userId), F.realTimeInstant)
      .mapN { (sett, time) =>
        (state.latestPrice, TradeStrategyExecutor.get(sett.strategy).analyze(state, trigger)).mapN { (price, result) =>
          val order = result match
            case Outcome.Buy   => sett.trading.toOrder(TradeOrder.Position.Buy)
            case Outcome.Sell  => sett.trading.toOrder(TradeOrder.Position.Sell)
            case Outcome.Close => TradeOrder.Exit
          TradeOrderPlacement(state.userId, state.currencyPair, order, sett.broker, price, time)
        }
      }
      .flatMap { // TODO: send request to broker, save order in repo, emit event to update state
        case None        => ().pure[F]
        case Some(order) => ().pure[F]
      }
}

object TradeService:
  def make[F[_]: Temporal](
      settingsRepo: TradeSettingsRepository[F],
      orderRepository: TradeOrderRepository[F],
      brokerClient: BrokerClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    Monad[F].pure(LiveTradeService[F](settingsRepo, orderRepository, brokerClient, dispatcher))
