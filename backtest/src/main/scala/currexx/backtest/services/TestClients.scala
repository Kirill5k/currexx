package currexx.backtest.services

import cats.Monad
import cats.effect.{Concurrent, Ref}
import cats.syntax.functor.*
import currexx.clients.broker.{BrokerClient, BrokerParameters}
import currexx.clients.data.MarketDataClient
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange, TradeOrder}

final class TestBrokerClient[F[_]](using F: Monad[F]) extends BrokerClient[F]:
  override def submit(pair: CurrencyPair, parameters: BrokerParameters, order: TradeOrder): F[Unit] = F.unit

final class TestMarketDataClient[F[_]](
    private val priceRef: Ref[F, Option[PriceRange]]
)(using F: Monad[F])
    extends MarketDataClient[F]:
  override def timeSeriesData(currencyPair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] = ???
  override def latestPrice(currencyPair: CurrencyPair): F[PriceRange]                                  = priceRef.get.map(_.get)

  def setPrice(price: PriceRange): F[Unit] = priceRef.set(Some(price))

final class TestClients[F[_]](
    val broker: TestBrokerClient[F],
    val data: TestMarketDataClient[F]
)

object TestClients {
  def make[F[_]: Concurrent]: F[TestClients[F]] =
    Ref.of[F, Option[PriceRange]](None).map { price =>
      TestClients[F](
        TestBrokerClient[F],
        TestMarketDataClient[F](price)
      )
    }
}
