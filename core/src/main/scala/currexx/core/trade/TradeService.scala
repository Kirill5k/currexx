package currexx.core.trade

import cats.Monad
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
import currexx.core.trade.TradeStrategyExecutor.Decision
import currexx.core.trade.db.{TradeOrderRepository, TradeSettingsRepository}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Indicator, Interval, TradeOrder}
import currexx.domain.user.UserId
import fs2.Stream

import java.time.Instant

trait TradeService[F[_]]:
  def getSettings(uid: UserId): F[TradeSettings]
  def updateSettings(settings: TradeSettings): F[Unit]
  def getAllOrders(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]]
  def processMarketStateUpdate(state: MarketState, triggers: List[Indicator]): F[Unit]
  def placeOrder(uid: UserId, order: TradeOrder, closePendingOrders: Boolean): F[Unit]
  def closeOpenOrders(uid: UserId): F[Unit]
  def closeOpenOrders(uid: UserId, cp: CurrencyPair): F[Unit]
  def closeOrderIfProfitIsOutsideRange(uid: UserId, cp: CurrencyPair, min: Option[BigDecimal], max: Option[BigDecimal]): F[Unit]
  def fetchMarketData(uid: UserId, cp: CurrencyPair, interval: Interval): F[Unit]

final private class LiveTradeService[F[_]](
    private val settingsRepository: TradeSettingsRepository[F],
    private val orderRepository: TradeOrderRepository[F],
    private val brokerClient: BrokerClient[F],
    private val marketDataClient: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F]
) extends TradeService[F] {
  override def getSettings(uid: UserId): F[TradeSettings]                                = settingsRepository.get(uid)
  override def updateSettings(settings: TradeSettings): F[Unit]                          = settingsRepository.update(settings)
  override def getAllOrders(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]] = orderRepository.getAll(uid, sp)

  override def fetchMarketData(uid: UserId, cp: CurrencyPair, interval: Interval): F[Unit] =
    marketDataClient.timeSeriesData(cp, interval).flatMap(data => dispatcher.dispatch(Action.ProcessMarketData(uid, data)))

  override def placeOrder(uid: UserId, order: TradeOrder, closePendingOrders: Boolean): F[Unit] =
    F.whenA(closePendingOrders)(closeOpenOrders(uid, order.currencyPair)) >>
      (F.realTimeInstant, settingsRepository.get(uid))
        .mapN((time, sett) => TradeOrderPlacement(uid, order, sett.broker, time))
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
          (F.realTimeInstant, marketDataClient.latestPrice(cp))
            .mapN((time, price) => top.copy(time = time, order = TradeOrder.Exit(cp, price.close)))
            .flatMap(submitOrderPlacement)
        case _ => F.unit
      }

  override def closeOrderIfProfitIsOutsideRange(
      uid: UserId,
      cp: CurrencyPair,
      min: Option[BigDecimal],
      max: Option[BigDecimal]
  ): F[Unit] =
    for
      settings   <- settingsRepository.get(uid)
      foundOrder <- brokerClient.find(settings.broker, cp)
      time       <- F.realTimeInstant
      _ <- foundOrder match
        case Some(o) if min.exists(_ > o.profit) || max.exists(_ < o.profit) =>
          submitOrderPlacement(TradeOrderPlacement(uid, TradeOrder.Exit(cp, o.currentPrice), settings.broker, time))
        case _ => F.unit
    yield ()

  override def processMarketStateUpdate(state: MarketState, triggers: List[Indicator]): F[Unit] =
    (settingsRepository.get(state.userId), marketDataClient.latestPrice(state.currencyPair), F.realTimeInstant)
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
          F.whenA(state.hasOpenPosition && top.order.isEnter)(brokerClient.submit(top.broker, TradeOrder.Exit(top.order.currencyPair, top.order.price))) *>
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
  def make[F[_]: Temporal](
      settingsRepo: TradeSettingsRepository[F],
      orderRepository: TradeOrderRepository[F],
      brokerClient: BrokerClient[F],
      marketDataClient: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    Monad[F].pure(LiveTradeService[F](settingsRepo, orderRepository, brokerClient, marketDataClient, dispatcher))
