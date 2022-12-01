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
import currexx.core.market.{MarketState, PositionState}
import currexx.core.settings.TradeParameters
import currexx.core.trade.TradeStrategyExecutor.Decision
import currexx.core.trade.db.{TradeOrderRepository, TradeSettingsRepository}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Indicator, IndicatorKind, Interval, TradeOrder}
import currexx.domain.monitor.Limits
import currexx.domain.time.Clock
import currexx.domain.user.UserId
import fs2.Stream

import java.time.Instant

trait TradeService[F[_]]:
  def getAllOrders(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]]
  def processMarketStateUpdate(state: MarketState, triggers: List[IndicatorKind]): F[Unit]
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
  override def getAllOrders(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]] = orderRepository.getAll(uid, sp)

  override def fetchMarketData(uid: UserId, cps: NonEmptyList[CurrencyPair], interval: Interval): F[Unit] =
    cps.traverse { cp =>
      marketDataClient
        .timeSeriesData(cp, interval)
        .flatMap(data => dispatcher.dispatch(Action.ProcessMarketData(uid, data)))
    }.void

  override def placeOrder(uid: UserId, order: TradeOrder, closePendingOrders: Boolean): F[Unit] =
    F.whenA(closePendingOrders)(closeOpenOrders(uid, order.currencyPair)) >>
      (clock.currentTime, settingsRepository.get(uid))
        .mapN((time, ts) => TradeOrderPlacement(uid, order, ts.broker, time))
        .flatMap(submitOrderPlacement)

  override def closeOpenOrders(uid: UserId): F[Unit] =
    Stream
      .evalSeq(orderRepository.getAllTradedCurrencies(uid))
      .evalMap(cp => closeOpenOrders(uid, cp))
      .compile
      .drain

  override def closeOpenOrders(uid: UserId, cp: CurrencyPair): F[Unit] =
    orderRepository
      .findLatestBy(uid, cp)
      .flatMap {
        case Some(top) if top.order.isEnter =>
          (clock.currentTime, marketDataClient.latestPrice(cp))
            .mapN((time, price) => top.copy(time = time, order = TradeOrder.Exit(cp, price.close)))
            .flatMap(submitOrderPlacement)
        case _ => F.unit
      }

  override def closeOrderIfProfitIsOutsideRange(uid: UserId, cps: NonEmptyList[CurrencyPair], limits: Limits): F[Unit] =
    for
      settings    <- settingsRepository.get(uid)
      foundOrders <- brokerClient.find(settings.broker, cps)
      time        <- clock.currentTime
      _ <- foundOrders
        .collect {
          case o if limits.max.exists(o.profit > _) && limits.trailing =>
            List(TradeOrder.Exit(o.currencyPair, o.currentPrice), TradeOrder.Enter(o.position, o.currencyPair, o.currentPrice, o.volume))
          case o if limits.min.exists(o.profit < _) || limits.max.exists(o.profit > _) =>
            List(TradeOrder.Exit(o.currencyPair, o.currentPrice))
        }
        .flatten
        .map(to => TradeOrderPlacement(uid, to, settings.broker, time))
        .traverse(submitOrderPlacement)
    yield ()

  override def processMarketStateUpdate(state: MarketState, triggers: List[IndicatorKind]): F[Unit] =
    (settingsRepository.get(state.userId), marketDataClient.latestPrice(state.currencyPair), clock.currentTime)
      .mapN { (settings, price, time) =>
        TradeStrategyExecutor
          .get(settings.strategy)
          .analyze(state, triggers)
          .map {
            case Decision.Buy   => settings.trading.toOrder(TradeOrder.Position.Buy, state.currencyPair, price.close)
            case Decision.Sell  => settings.trading.toOrder(TradeOrder.Position.Sell, state.currencyPair, price.close)
            case Decision.Close => TradeOrder.Exit(state.currencyPair, price.close)
          }
          .map(order => TradeOrderPlacement(state.userId, order, settings.broker, time))
      }
      .flatMap {
        case Some(top) =>
          F.whenA(state.hasOpenPosition && top.order.isEnter)(
            brokerClient.submit(top.broker, TradeOrder.Exit(top.order.currencyPair, top.order.price))
          ) *>
            submitOrderPlacement(top)
        case None =>
          F.unit
      }

  private def submitOrderPlacement(top: TradeOrderPlacement): F[Unit] =
    brokerClient.submit(top.broker, top.order) *>
      orderRepository.save(top) *>
      dispatcher.dispatch(Action.ProcessTradeOrderPlacement(top))

  extension (ms: MarketState) def hasOpenPosition: Boolean = ms.currentPosition.isDefined
}

object TradeService:
  def make[F[_]: Temporal: Clock](
      settingsRepo: TradeSettingsRepository[F],
      orderRepository: TradeOrderRepository[F],
      brokerClient: BrokerClient[F],
      marketDataClient: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    Monad[F].pure(LiveTradeService[F](settingsRepo, orderRepository, brokerClient, marketDataClient, dispatcher))
