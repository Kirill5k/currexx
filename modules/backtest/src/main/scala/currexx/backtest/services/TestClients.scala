package currexx.backtest.services

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.{Concurrent, Ref}
import cats.syntax.functor.*
import currexx.clients.broker.{BrokerClient, BrokerParameters}
import currexx.clients.data.MarketDataClient
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, OpenedTradeOrder, OrderPlacementStatus, PriceRange, TradeOrder}

final class TestBrokerClient[F[_]](using F: Monad[F]) extends BrokerClient[F]:
  override def find(parameters: BrokerParameters, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] = F.pure(Nil)
  override def submit(parameters: BrokerParameters, order: TradeOrder): F[OrderPlacementStatus] = F.pure(OrderPlacementStatus.Success)

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
  def make[F[_]: Concurrent]: F[TestClients[F]] =
    Ref.of[F, Option[MarketTimeSeriesData]](None).map { price =>
      TestClients[F](
        TestBrokerClient[F],
        TestMarketDataClient[F](price)
      )
    }
}
