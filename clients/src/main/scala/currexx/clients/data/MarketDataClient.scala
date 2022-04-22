package currexx.clients.data

import currexx.clients.HttpClient
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData}

trait MarketDataClient[F[_]] extends HttpClient[F]:
  def timeSeriesData(currencyPair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData]
