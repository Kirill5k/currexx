package currexx.clients.data

import cats.{Monad, MonadThrow}
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
    oandaDataClient.latestPrice(cp)
      .handleErrorWith(_ => twelveDataClient.latestPrice(cp))

  def timeSeriesData(cp: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] =
    twelveDataClient.timeSeriesData(cp, interval)
      .handleErrorWith(_ => oandaDataClient.timeSeriesData(cp, interval))
}

object MarketDataClient:
  def make[F[_]: MonadThrow](
      twelveDataClient: TwelveDataClient[F],
      oandaDataClient: OandaDataClient[F],
  ): F[MarketDataClient[F]] =
    Monad[F].pure(LiveMarketDataClient[F](twelveDataClient, oandaDataClient))
