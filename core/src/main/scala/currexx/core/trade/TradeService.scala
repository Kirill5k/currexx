package currexx.core.trade

import cats.Monad
import cats.effect.kernel.Temporal
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.clients.broker.BrokerClient
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.market.MarketState
import currexx.core.trade.TradeStrategyExecutor.Decision
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
      .mapN { (settings, time) =>
        (state.latestPrice, TradeStrategyExecutor.get(settings.strategy).analyze(state, trigger)).mapN { (price, result) =>
          val order = result match
            case Decision.Buy   => settings.trading.toOrder(TradeOrder.Position.Buy)
            case Decision.Sell  => settings.trading.toOrder(TradeOrder.Position.Sell)
            case Decision.Close => TradeOrder.Exit
          TradeOrderPlacement(state.userId, state.currencyPair, order, settings.broker, price, time)
        }
      }
      .flatMap {
        case Some(order) =>
          F.whenA(state.currentPosition.exists(order.isReverse))(brokerClient.submit(order.currencyPair, order.broker, TradeOrder.Exit)) *>
            brokerClient.submit(order.currencyPair, order.broker, order.order) *>
            orderRepository.save(order) *>
            dispatcher.dispatch(Action.ProcessTradeOrderPlacement(order))
        case None =>
          F.unit
      }

  extension (top: TradeOrderPlacement)
    def isReverse(currentPosition: TradeOrder.Position): Boolean =
      top.order match
        case TradeOrder.Enter(position, _, _, _, _) => position != currentPosition
        case TradeOrder.Exit                        => false
}

object TradeService:
  def make[F[_]: Temporal](
      settingsRepo: TradeSettingsRepository[F],
      orderRepository: TradeOrderRepository[F],
      brokerClient: BrokerClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    Monad[F].pure(LiveTradeService[F](settingsRepo, orderRepository, brokerClient, dispatcher))
