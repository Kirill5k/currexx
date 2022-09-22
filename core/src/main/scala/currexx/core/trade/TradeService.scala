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
  def placeOrder(uid: UserId, cp: CurrencyPair, order: TradeOrder, closePendingOrders: Boolean): F[Unit]
  def closeOpenOrders(uid: UserId): F[Unit]
  def closeOpenOrders(uid: UserId, cp: CurrencyPair): F[Unit]
  def closeOpenOrdersIfProfitIsOutsideRange(uid: UserId, cp: CurrencyPair, min: Option[BigDecimal], max: Option[BigDecimal]): F[Unit]
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

  override def placeOrder(uid: UserId, cp: CurrencyPair, order: TradeOrder, closePendingOrders: Boolean): F[Unit] =
    (F.realTimeInstant, marketDataClient.latestPrice(cp), settingsRepository.get(uid))
      .mapN { (time, price, sett) =>
        val pendingClose = if (closePendingOrders && order.isEnter) {
          orderRepository
            .findLatestBy(uid, cp)
            .map {
              case Some(top) if top.order.isEnter =>
                Some(top.copy(time = time, price = price.close, order = TradeOrder.Exit))
              case _ => None
            }
        } else F.pure(None)

        pendingClose.map(_.toVector :+ TradeOrderPlacement(uid, cp, order, sett.broker, price.close, time))
      }
      .flatten
      .flatMap(_.traverse(submitOrderPlacement).void)

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
        case Some(top) if top.order.isEnter =>
          (F.realTimeInstant, marketDataClient.latestPrice(cp))
            .mapN((time, price) => top.copy(time = time, price = price.close, order = TradeOrder.Exit))
            .flatMap(submitOrderPlacement)
        case _ => F.unit
      }

  override def closeOpenOrdersIfProfitIsOutsideRange(
      uid: UserId,
      cp: CurrencyPair,
      min: Option[BigDecimal],
      max: Option[BigDecimal]
  ): F[Unit] =
    for
      settings    <- settingsRepository.get(uid)
      foundOrder  <- brokerClient.find(cp, settings.broker)
      openedOrder <- F.fromOption(foundOrder, AppError.NoOpenedPositions(uid, cp, settings.broker.broker.print))
      time        <- F.realTimeInstant
      _ <- F.whenA(min.exists(_ > openedOrder.profit) || max.exists(_ < openedOrder.profit)) {
        submitOrderPlacement(TradeOrderPlacement(uid, cp, TradeOrder.Exit, settings.broker, openedOrder.currentPrice, time))
      }
    yield ()

  override def processMarketStateUpdate(state: MarketState, triggers: List[Indicator]): F[Unit] =
    (settingsRepository.get(state.userId), marketDataClient.latestPrice(state.currencyPair), F.realTimeInstant)
      .mapN { (settings, price, time) =>
        TradeStrategyExecutor
          .get(settings.strategy)
          .analyze(state, triggers)
          .map {
            case Decision.Buy   => settings.trading.toOrder(state.currencyPair, TradeOrder.Position.Buy)
            case Decision.Sell  => settings.trading.toOrder(state.currencyPair, TradeOrder.Position.Sell)
            case Decision.Close => TradeOrder.Exit
          }
          .map(order => TradeOrderPlacement(state.userId, state.currencyPair, order, settings.broker, price.close, time))
      }
      .flatMap {
        case Some(top) =>
          F.whenA(state.hasOpenPosition && top.order.isEnter)(brokerClient.submit(top.currencyPair, top.broker, TradeOrder.Exit)) *>
            submitOrderPlacement(top)
        case None =>
          F.unit
      }

  private def submitOrderPlacement(top: TradeOrderPlacement): F[Unit] =
    brokerClient.submit(top.currencyPair, top.broker, top.order) *>
      orderRepository.save(top) *>
      dispatcher.dispatch(Action.ProcessTradeOrderPlacement(top))

  extension (ms: MarketState) def hasOpenPosition: Boolean = ms.currentPosition.isDefined

  extension (to: TradeOrder)
    def isEnter: Boolean =
      to match
        case TradeOrder.Exit     => false
        case _: TradeOrder.Enter => true
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
