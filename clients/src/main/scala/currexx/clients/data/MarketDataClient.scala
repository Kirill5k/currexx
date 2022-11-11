package currexx.clients.data

import cats.{Monad, MonadThrow}
import cats.syntax.applicativeError.*
import currexx.clients.HttpClient
import currexx.clients.data.alphavantage.AlphaVantageClient
import currexx.clients.data.twelvedata.TwelveDataClient
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}

trait MarketDataClient[F[_]]:
  def timeSeriesData(currencyPair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData]
  def latestPrice(currencyPair: CurrencyPair): F[PriceRange]

final private class LiveMarketDataClient[F[_]: MonadThrow](
    private val alphaVantageClient: AlphaVantageClient[F],
    private val twelveDataClient: TwelveDataClient[F]
) extends MarketDataClient[F] {
  def latestPrice(cp: CurrencyPair): F[PriceRange] =
    twelveDataClient
      .latestPrice(cp)
      .handleErrorWith(_ => alphaVantageClient.latestPrice(cp))

  def timeSeriesData(cp: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] =
    twelveDataClient
      .timeSeriesData(cp, interval)
      .handleErrorWith(_ => alphaVantageClient.timeSeriesData(cp, interval))
}

object MarketDataClient:
  def make[F[_]: MonadThrow](
      alphaVantageClient: AlphaVantageClient[F],
      twelveDataClient: TwelveDataClient[F]
  ): F[MarketDataClient[F]] =
    Monad[F].pure(LiveMarketDataClient[F](alphaVantageClient, twelveDataClient))
