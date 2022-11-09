package currexx.clients.data

import cats.Monad
import currexx.clients.HttpClient
import currexx.clients.data.alphavantage.AlphaVantageClient
import currexx.clients.data.twelvedata.TwelveDataClient
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}

trait MarketDataClient[F[_]]:
  def timeSeriesData(currencyPair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData]
  def latestPrice(currencyPair: CurrencyPair): F[PriceRange]

final private class LiveMarketDataClient[F[_]](
    private val alphaVantageClient: AlphaVantageClient[F],
    private val twelveDataClient: TwelveDataClient[F],
) extends MarketDataClient[F]:
  def latestPrice(currencyPair: CurrencyPair): F[PriceRange] =
    twelveDataClient.latestPrice(currencyPair)

  def timeSeriesData(currencyPair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] =
    twelveDataClient.timeSeriesData(currencyPair, interval)

object MarketDataClient:
  def make[F[_]: Monad](
      alphaVantageClient: AlphaVantageClient[F],
      twelveDataClient: TwelveDataClient[F]
  ): F[MarketDataClient[F]] =
    Monad[F].pure(LiveMarketDataClient[F](alphaVantageClient, twelveDataClient))
