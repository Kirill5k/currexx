package currexx.backtest.services

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.{Concurrent, Ref}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.broker.{BrokerClient, BrokerParameters}
import currexx.clients.data.MarketDataClient
import currexx.domain.market.{
  CurrencyPair,
  Interval,
  MarketTimeSeriesData,
  OpenedTradeOrder,
  OrderExecution,
  OrderPlacementResult,
  PriceRange,
  TradeOrder
}
import kirill5k.common.cats.Clock

final class TestBrokerClient[F[_]](
    clock: Clock[F],
    positions: Ref[F, Map[CurrencyPair, OpenedTradeOrder]]
)(using F: Monad[F])
    extends BrokerClient[F]:
  override def find(parameters: BrokerParameters, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] =
    positions.get.map(opened => cps.toList.flatMap(opened.get))
  override def findEntryExecutions(
      parameters: BrokerParameters,
      cp: CurrencyPair,
      position: TradeOrder.Position
  ): F[List[OrderExecution]] = F.pure(Nil)

  // Orders fill instantly at their requested price, and an entry replaces any position held in its currency pair
  override def submit(parameters: BrokerParameters, order: TradeOrder): F[OrderPlacementResult] =
    for
      time   <- clock.now
      volume <- positions.modify { opened =>
        order match
          case enter: TradeOrder.Enter =>
            val position = OpenedTradeOrder(enter.currencyPair, enter.position, enter.price, enter.price, enter.volume, BigDecimal(0))
            (opened.updated(enter.currencyPair, position), enter.volume)
          case exit: TradeOrder.Exit =>
            (opened - exit.currencyPair, opened.get(exit.currencyPair).fold(BigDecimal(0))(_.volume))
      }
    yield OrderPlacementResult.filled(OrderExecution(order.price, time, volume, orderId = "", transactionId = "", tradeIds = Nil))

final class TestMarketDataClient[F[_]](
    private val priceRef: Ref[F, Option[MarketTimeSeriesData]]
)(using F: Monad[F])
    extends MarketDataClient[F]:
  override def timeSeriesData(currencyPair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] = priceRef.get.map(_.get)
  override def latestPrice(currencyPair: CurrencyPair): F[PriceRange]                                  = priceRef.get.map(_.get.prices.head)

  def setData(tsd: MarketTimeSeriesData): F[Unit] = priceRef.set(Some(tsd))

final class TestClients[F[_]](
    val broker: TestBrokerClient[F],
    val data: TestMarketDataClient[F]
)

object TestClients {
  def make[F[_]: Concurrent](clock: Clock[F]): F[TestClients[F]] =
    for
      price     <- Ref.of[F, Option[MarketTimeSeriesData]](None)
      positions <- Ref.of[F, Map[CurrencyPair, OpenedTradeOrder]](Map.empty)
    yield TestClients[F](
      TestBrokerClient[F](clock, positions),
      TestMarketDataClient[F](price)
    )
}
