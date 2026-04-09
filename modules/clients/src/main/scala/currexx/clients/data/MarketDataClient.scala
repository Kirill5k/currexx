package currexx.clients.data

import cats.MonadThrow
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import currexx.clients.data.twelvedata.TwelveDataClient
import currexx.clients.data.oanda.OandaDataClient
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}

trait MarketDataClient[F[_]]:
  def timeSeriesData(currencyPair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData]
  def latestPrice(currencyPair: CurrencyPair): F[PriceRange]

final private class LiveMarketDataClient[F[_]: MonadThrow](
    private val twelveDataClient: TwelveDataClient[F],
    private val oandaDataClient: OandaDataClient[F]
) extends MarketDataClient[F] {
  def latestPrice(cp: CurrencyPair): F[PriceRange] =
    twelveDataClient.latestPrice(cp)
      .handleErrorWith(_ => oandaDataClient.latestPrice(cp))

  def timeSeriesData(cp: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] =
    oandaDataClient.timeSeriesData(cp, interval)
      .handleErrorWith(_ => twelveDataClient.timeSeriesData(cp, interval))
}

object MarketDataClient:
  def make[F[_]: MonadThrow](
      twelveDataClient: TwelveDataClient[F],
      oandaDataClient: OandaDataClient[F],
  ): F[MarketDataClient[F]] =
    LiveMarketDataClient[F](twelveDataClient, oandaDataClient).pure[F]
