package currexx.core.trade

import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.data.NonEmptyList
import cats.effect.implicits.parallelForGenSpawn
import cats.effect.kernel.Temporal
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import cats.syntax.parallel.*
import currexx.clients.broker.BrokerClient
import currexx.clients.data.MarketDataClient
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.common.effects.*
import currexx.core.market.{MarketProfile, MarketState}
import currexx.core.settings.TradeSettings
import currexx.core.trade.TradeAction
import currexx.core.trade.db.{OrderStatusRepository, TradeOrderRepository, TradeSettingsRepository}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Interval, OrderPlacementStatus, TradeOrder}
import currexx.domain.monitor.Limits
import kirill5k.common.cats.Clock
import currexx.domain.user.UserId
import fs2.Stream
import org.typelevel.log4cats.Logger

import java.time.Instant

trait TradeService[F[_]]:
  def getAllOrders(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]]
  def getOrderStatistics(uid: UserId, sp: SearchParams): F[OrderStatistics]
  def processMarketStateUpdate(state: MarketState): F[Unit]
  def placeOrder(uid: UserId, order: TradeOrder, closePendingOrders: Boolean): F[Unit]
  def closeOpenOrders(uid: UserId): F[Unit]
  def closeOpenOrders(uid: UserId, cp: CurrencyPair): F[Unit]
  def closeOrderIfProfitIsOutsideRange(uid: UserId, cps: NonEmptyList[CurrencyPair], limits: Limits): F[Unit]
  def fetchMarketData(uid: UserId, cps: NonEmptyList[CurrencyPair], interval: Interval): F[Unit]

final private class LiveTradeService[F[_]](
    private val settingsRepository: TradeSettingsRepository[F],
    private val orderRepository: TradeOrderRepository[F],
    private val orderStatusRepository: OrderStatusRepository[F],
    private val brokerClient: BrokerClient[F],
    private val marketDataClient: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F],
    clock: Clock[F],
    logger: Logger[F]
) extends TradeService[F] {
  override def getAllOrders(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]] =
    orderRepository.getAll(uid, sp)

  override def getOrderStatistics(uid: UserId, sp: SearchParams): F[OrderStatistics] =
    orderStatusRepository.getStatistics(uid, sp)

  override def fetchMarketData(uid: UserId, cps: NonEmptyList[CurrencyPair], interval: Interval): F[Unit] =
    cps.parTraverse_ { cp =>
      marketDataClient
        .timeSeriesData(cp, interval)
        .map(Action.ProcessMarketData(uid, _))
        .flatMap(dispatcher.dispatch)
    }

  override def placeOrder(uid: UserId, order: TradeOrder, closePendingOrders: Boolean): F[Unit] =
    for
      closeStatus <- if closePendingOrders then closeOpenOrder(uid, order.currencyPair) else F.pure(OrderPlacementStatus.NoPosition)
      _           <- F.raiseUnless(canOpenAfterClose(closeStatus)) {
        val reason = closeStatus match
          case OrderPlacementStatus.Cancelled(reason) => s"prerequisite close was cancelled by broker: $reason"
          case _                                      => "prerequisite close is still pending"
        AppError.OrderPlacementBlocked(order.currencyPair, reason)
      }
      ts     <- settingsRepository.get(uid)
      time   <- clock.now
      status <- submitOrderPlacement(TradeOrderPlacement(uid, order, ts.broker, time))
      _      <- status match
        case OrderPlacementStatus.Cancelled(reason) => F.raiseError[Unit](AppError.OrderPlacementCancelled(order.currencyPair, reason))
        case _                                      => F.unit
    yield ()

  override def closeOpenOrders(uid: UserId): F[Unit] =
    Stream
      .evalSeq(orderRepository.getAllTradedCurrencies(uid))
      .evalMap(cp => closeOpenOrders(uid, cp))
      .compile
      .drain

  override def closeOpenOrders(uid: UserId, cp: CurrencyPair): F[Unit] =
    closeOpenOrder(uid, cp).void

  private def closeOpenOrder(uid: UserId, cp: CurrencyPair): F[OrderPlacementStatus] =
    orderRepository
      .findLatestBy(uid, cp)
      .flatmapOpt(F.pure[OrderPlacementStatus](OrderPlacementStatus.NoPosition)) { top =>
        if top.order.isEnter then
          for
            time   <- clock.now
            price  <- marketDataClient.latestPrice(cp)
            status <- submitOrderPlacement(top.copy(time = time, order = TradeOrder.Exit(cp, price.close)))
          yield status
        else F.pure(OrderPlacementStatus.NoPosition)
      }

  private def canOpenAfterClose(status: OrderPlacementStatus): Boolean = status match
    case OrderPlacementStatus.Success | OrderPlacementStatus.NoPosition => true
    case _                                                              => false

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
        .traverse(to => submitOrderPlacement(TradeOrderPlacement(uid, to, settings.broker, time)))
    yield ()

  override def processMarketStateUpdate(state: MarketState): F[Unit] = {
    val previousProfile = state.previousProfile.getOrElse(MarketProfile())
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
      _ <- finalAction.traverse_(executeAction(_, state, settings))
    yield ()
  }

  private def executeAction(action: TradeAction, state: MarketState, settings: TradeSettings): F[Unit] = {
    def submit(order: TradeOrder, time: Instant): F[OrderPlacementStatus] =
      submitOrderPlacement(TradeOrderPlacement(state.userId, order, settings.broker, time))

    def open(position: TradeOrder.Position, price: BigDecimal, time: Instant, closeFirst: Boolean): F[Unit] = {
      val openOrder = settings.trading.toOrder(position, state.currencyPair, price)
      brokerClient.find(settings.broker, NonEmptyList.one(state.currencyPair)).flatMap { openedOrders =>
        // An action retried after its entry was filled but not recorded must neither close nor repeat that entry
        openedOrders.find(_.position == position) match
          case Some(opened) =>
            val filledOrder = TradeOrder.Enter(position, state.currencyPair, opened.openPrice, opened.volume)
            recordOrderPlacement(TradeOrderPlacement(state.userId, filledOrder, settings.broker, time), OrderPlacementStatus.Success)
          case None if closeFirst =>
            val exit                 = TradeOrderPlacement(state.userId, TradeOrder.Exit(state.currencyPair, price), settings.broker, time)
            def publishExit: F[Unit] = dispatcher.dispatch(Action.ProcessTradeOrderPlacement(exit))
            submitOrderPlacement(exit, skipEvent = true).flatMap { closeStatus =>
              F.whenA(canOpenAfterClose(closeStatus)) {
                // ActionProcessor handles events concurrently, so publishing both could apply the exit after the entry.
                // Publish only the entry on success, or the confirmed exit if opening fails.
                submit(openOrder, time)
                  .onError { case _ => publishExit }
                  .flatMap {
                    case OrderPlacementStatus.Cancelled(_) => publishExit
                    case _                                 => F.unit
                  }
              }
            }
          case None => submit(openOrder, time).void
      }
    }

    for
      time  <- clock.now
      price <- marketDataClient.latestPrice(state.currencyPair)
      _     <- action match
        case TradeAction.OpenLong      => open(TradeOrder.Position.Buy, price.close, time, closeFirst = false)
        case TradeAction.FlipToLong    => open(TradeOrder.Position.Buy, price.close, time, closeFirst = true)
        case TradeAction.OpenShort     => open(TradeOrder.Position.Sell, price.close, time, closeFirst = false)
        case TradeAction.FlipToShort   => open(TradeOrder.Position.Sell, price.close, time, closeFirst = true)
        case TradeAction.ClosePosition => submit(TradeOrder.Exit(state.currencyPair, price.close), time).void
    yield ()
  }

  private def submitOrderPlacement(top: TradeOrderPlacement, skipEvent: Boolean = false): F[OrderPlacementStatus] =
    brokerClient.submit(top.broker, top.order).flatTap(recordOrderPlacement(top, _, skipEvent))

  private def recordOrderPlacement(top: TradeOrderPlacement, status: OrderPlacementStatus, skipEvent: Boolean = false): F[Unit] =
    for
      _ <- orderStatusRepository.save(top, status)
      _ <- status match
        case OrderPlacementStatus.Pending if !top.order.isEnter =>
          logger.warn(s"Close order is pending at broker: ${top.order}")
        case OrderPlacementStatus.Success | OrderPlacementStatus.Pending =>
          orderRepository.save(top) *>
            F.whenA(!skipEvent)(dispatcher.dispatch(Action.ProcessTradeOrderPlacement(top)))
        case OrderPlacementStatus.Cancelled(reason) =>
          logger.warn(s"Order was cancelled by broker: ${top.order} - Reason: $reason")
        case OrderPlacementStatus.NoPosition =>
          logger.warn(s"Order skipped, no open position to close: ${top.order}") *>
            F.whenA(!skipEvent)(dispatcher.dispatch(Action.ProcessTradeOrderPlacement(top)))
    yield ()
}

object TradeService:
  def make[F[_]: {Temporal, Clock, Logger}](
      settingsRepo: TradeSettingsRepository[F],
      orderRepository: TradeOrderRepository[F],
      orderStatusRepository: OrderStatusRepository[F],
      brokerClient: BrokerClient[F],
      marketDataClient: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    LiveTradeService[F](settingsRepo, orderRepository, orderStatusRepository, brokerClient, marketDataClient, dispatcher).pure[F]
