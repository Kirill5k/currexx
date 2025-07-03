package currexx.core.trade

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.kernel.Temporal
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import currexx.clients.broker.BrokerClient
import currexx.clients.data.MarketDataClient
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.common.effects.*
import currexx.core.market.{MarketProfile, MarketState}
import currexx.core.settings.TradeSettings
import currexx.core.trade.TradeAction
import currexx.core.trade.db.{TradeOrderRepository, TradeSettingsRepository}
import currexx.domain.market.{CurrencyPair, Interval, TradeOrder}
import currexx.domain.monitor.Limits
import kirill5k.common.cats.Clock
import currexx.domain.user.UserId
import fs2.Stream

import java.time.Instant

trait TradeService[F[_]]:
  def getAllOrders(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]]
  def processMarketStateUpdate(state: MarketState, previousProfile: MarketProfile): F[Unit]
  def placeOrder(uid: UserId, order: TradeOrder, closePendingOrders: Boolean): F[Unit]
  def closeOpenOrders(uid: UserId): F[Unit]
  def closeOpenOrders(uid: UserId, cp: CurrencyPair): F[Unit]
  def closeOrderIfProfitIsOutsideRange(uid: UserId, cps: NonEmptyList[CurrencyPair], limits: Limits): F[Unit]
  def fetchMarketData(uid: UserId, cps: NonEmptyList[CurrencyPair], interval: Interval): F[Unit]

final private class LiveTradeService[F[_]](
    private val settingsRepository: TradeSettingsRepository[F],
    private val orderRepository: TradeOrderRepository[F],
    private val brokerClient: BrokerClient[F],
    private val marketDataClient: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F],
    clock: Clock[F]
) extends TradeService[F] {
  override def getAllOrders(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]] =
    orderRepository.getAll(uid, sp)

  override def fetchMarketData(uid: UserId, cps: NonEmptyList[CurrencyPair], interval: Interval): F[Unit] =
    cps.traverse { cp =>
      marketDataClient
        .timeSeriesData(cp, interval)
        .map(Action.ProcessMarketData(uid, _))
        .flatMap(dispatcher.dispatch)
    }.void

  override def placeOrder(uid: UserId, order: TradeOrder, closePendingOrders: Boolean): F[Unit] =
    for
      _    <- F.whenA(closePendingOrders)(closeOpenOrders(uid, order.currencyPair))
      ts   <- settingsRepository.get(uid)
      time <- clock.now
      _    <- submitOrderPlacement(TradeOrderPlacement(uid, order, ts.broker, time))
    yield ()

  override def closeOpenOrders(uid: UserId): F[Unit] =
    Stream
      .evalSeq(orderRepository.getAllTradedCurrencies(uid))
      .evalMap(cp => closeOpenOrders(uid, cp))
      .compile
      .drain

  override def closeOpenOrders(uid: UserId, cp: CurrencyPair): F[Unit] =
    orderRepository
      .findLatestBy(uid, cp)
      .flatMapOption(F.unit) { top =>
        F.whenA(top.order.isEnter) {
          for
            time  <- clock.now
            price <- marketDataClient.latestPrice(cp)
            order = top.copy(time = time, order = TradeOrder.Exit(cp, price.close))
            _ <- submitOrderPlacement(order)
          yield ()
        }
      }

  override def closeOrderIfProfitIsOutsideRange(uid: UserId, cps: NonEmptyList[CurrencyPair], limits: Limits): F[Unit] =
    for
      settings    <- settingsRepository.get(uid)
      foundOrders <- brokerClient.find(settings.broker, cps)
      time        <- clock.now
      _           <- foundOrders
        .collect {
          case o if limits.min.exists(o.profit < _) || limits.max.exists(o.profit > _) =>
            TradeOrder.Exit(o.currencyPair, o.currentPrice)
        }
        .map(to => TradeOrderPlacement(uid, to, settings.broker, time))
        .traverse(submitOrderPlacement)
    yield ()

  override def processMarketStateUpdate(state: MarketState, previousProfile: MarketProfile): F[Unit] =
    for
      settings <- settingsRepository.get(state.userId)
      closeAction = Rule.findTriggeredAction(settings.strategy.closeRules, state, previousProfile)
      openAction  = Rule.findTriggeredAction(settings.strategy.openRules, state, previousProfile)
      finalAction = (state.currentPosition, closeAction, openAction) match {
        // --- Case 1: We are in a position AND a close rule was triggered. ---
        // The close rule takes highest priority. We exit the position.
        case (Some(_), Some(action), _) => Some(action) // `action` will be `ClosePosition`
        // --- Case 2: We are in a position AND an *opposite* open rule was triggered. ---
        // This is the "Stop and Reverse" (SAR) logic.
        case (Some(pos), None, Some(TradeAction.OpenShort)) if pos.position == TradeOrder.Position.Buy =>
          // We are long, but an OpenShort signal appeared. We need a new "FlipToShort" action.
          Some(TradeAction.FlipToShort)
        case (Some(pos), None, Some(TradeAction.OpenLong)) if pos.position == TradeOrder.Position.Sell =>
          // We are short, but an OpenLong signal appeared.
          Some(TradeAction.FlipToLong)
        // --- Case 3: We are flat AND an open rule was triggered. ---
        case (None, _, Some(action)) => Some(action) // `action` will be `OpenLong` or `OpenShort`
        // --- Default Case: No action to be taken ---
        case _ => None
      }
      _ <- F.whenA(finalAction.isDefined)(executeAction(finalAction.get, state, settings))
    yield ()

  private def executeAction(action: TradeAction, state: MarketState, settings: TradeSettings): F[Unit] = {
    def submit(order: TradeOrder, time: Instant): F[Unit] =
      submitOrderPlacement(TradeOrderPlacement(state.userId, order, settings.broker, time))
    for
      time  <- clock.now
      price <- marketDataClient.latestPrice(state.currencyPair)
      _     <- action match
        case TradeAction.OpenLong =>
          val order = settings.trading.toOrder(TradeOrder.Position.Buy, state.currencyPair, price.close)
          submit(order, time)

        case TradeAction.FlipToLong =>
          val exitOrder = TradeOrder.Exit(state.currencyPair, price.close)
          val openOrder = settings.trading.toOrder(TradeOrder.Position.Buy, state.currencyPair, price.close)
          submit(exitOrder, time) >> submit(openOrder, time)

        case TradeAction.OpenShort =>
          val order = settings.trading.toOrder(TradeOrder.Position.Sell, state.currencyPair, price.close)
          submit(order, time)

        case TradeAction.FlipToShort =>
          val exitOrder = TradeOrder.Exit(state.currencyPair, price.close)
          val openOrder = settings.trading.toOrder(TradeOrder.Position.Sell, state.currencyPair, price.close)
          submit(exitOrder, time) >> submit(openOrder, time)

        case TradeAction.ClosePosition =>
          val order = TradeOrder.Exit(state.currencyPair, price.close)
          submit(order, time)
    yield ()
  }

  private def submitOrderPlacement(top: TradeOrderPlacement): F[Unit] =
    brokerClient.submit(top.broker, top.order) *>
      orderRepository.save(top) *>
      dispatcher.dispatch(Action.ProcessTradeOrderPlacement(top))
}

object TradeService:
  def make[F[_]: {Temporal, Clock}](
      settingsRepo: TradeSettingsRepository[F],
      orderRepository: TradeOrderRepository[F],
      brokerClient: BrokerClient[F],
      marketDataClient: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    Monad[F].pure(LiveTradeService[F](settingsRepo, orderRepository, brokerClient, marketDataClient, dispatcher))
