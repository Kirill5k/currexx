package currexx.clients

import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData}

final case class MarketDataClientConfig(
    baseUri: String,
    apiKey: String
)

trait MarketDataClient[F[_]] extends HttpClient[F]:
  def timeSeriesData(currencyPair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData]
