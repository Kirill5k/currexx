package currexx.core.trade

import cats.Monad
import cats.effect.kernel.Temporal
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.clients.broker.BrokerClient
import currexx.clients.data.MarketDataClient
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.market.{MarketState, PositionState}
import currexx.core.trade.TradeStrategyExecutor.Decision
import currexx.core.trade.db.{TradeOrderRepository, TradeSettingsRepository}
import currexx.domain.market.{CurrencyPair, Indicator, TradeOrder}
import currexx.domain.user.UserId
import fs2.Stream

trait TradeService[F[_]]:
  def getSettings(uid: UserId): F[TradeSettings]
  def updateSettings(settings: TradeSettings): F[Unit]
  def getAllOrders(uid: UserId): F[List[TradeOrderPlacement]]
  def processMarketStateUpdate(state: MarketState, trigger: Indicator): F[Unit]
  def closeOpenOrders(uid: UserId, cp: CurrencyPair): F[Unit]
  def closeOpenOrders(uid: UserId): F[Unit]

final private class LiveTradeService[F[_]](
    private val settingsRepository: TradeSettingsRepository[F],
    private val orderRepository: TradeOrderRepository[F],
    private val brokerClient: BrokerClient[F],
    private val marketDataClient: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F]
) extends TradeService[F] {
  override def getSettings(uid: UserId): F[TradeSettings]              = settingsRepository.get(uid)
  override def updateSettings(settings: TradeSettings): F[Unit]        = settingsRepository.update(settings)
  override def getAllOrders(uid: UserId): F[List[TradeOrderPlacement]] = orderRepository.getAll(uid)

  override def closeOpenOrders(uid: UserId): F[Unit] =
    Stream
      .evalSeq(orderRepository.getAllTradedCurrencies(uid))
      .map(cp => Stream.eval(closeOpenOrders(uid, cp)))
      .parJoinUnbounded
      .compile
      .drain

  override def closeOpenOrders(uid: UserId, cp: CurrencyPair): F[Unit] =
    orderRepository
      .findLatestBy(uid, cp)
      .flatMap {
        case None                                                      => F.unit
        case Some(TradeOrderPlacement(_, _, TradeOrder.Exit, _, _, _)) => F.unit
        case Some(top) =>
          (F.realTimeInstant, marketDataClient.latestPrice(cp))
            .mapN((time, price) => top.copy(time = time, currentPrice = price, order = TradeOrder.Exit))
            .flatMap(submitOrderPlacement)
      }

  override def processMarketStateUpdate(state: MarketState, trigger: Indicator): F[Unit] =
    (settingsRepository.get(state.userId), F.realTimeInstant)
      .mapN { (settings, time) =>
        (state.latestPrice, TradeStrategyExecutor.get(settings.strategy).analyze(state, trigger)).mapN { (price, result) =>
          val order = result match
            case Decision.Buy   => settings.trading.toOrder(state.currencyPair, TradeOrder.Position.Buy)
            case Decision.Sell  => settings.trading.toOrder(state.currencyPair, TradeOrder.Position.Sell)
            case Decision.Close => TradeOrder.Exit
          TradeOrderPlacement(state.userId, state.currencyPair, order, settings.broker, price, time)
        }
      }
      .flatMap {
        case Some(top) =>
          F.whenA(state.currentPosition.exists(top.isReverse))(brokerClient.submit(top.currencyPair, top.broker, TradeOrder.Exit)) *>
            submitOrderPlacement(top)
        case None =>
          F.unit
      }

  private def submitOrderPlacement(top: TradeOrderPlacement): F[Unit] =
    brokerClient.submit(top.currencyPair, top.broker, top.order) *>
      orderRepository.save(top) *>
      dispatcher.dispatch(Action.ProcessTradeOrderPlacement(top))

  extension (top: TradeOrderPlacement)
    def isReverse(currentPosition: PositionState): Boolean =
      top.order match
        case order: TradeOrder.Enter => order.position != currentPosition.position
        case TradeOrder.Exit         => false
}

object TradeService:
  def make[F[_]: Temporal](
      settingsRepo: TradeSettingsRepository[F],
      orderRepository: TradeOrderRepository[F],
      brokerClient: BrokerClient[F],
      marketDataClient: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    Monad[F].pure(LiveTradeService[F](settingsRepo, orderRepository, brokerClient, marketDataClient, dispatcher))
