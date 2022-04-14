package currexx.clients

import cats.effect.Temporal
import currexx.clients.alphavantage.AlphaVantageClient
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData}
import org.typelevel.log4cats.Logger
import sttp.client3.SttpBackend

final case class MarketDataClientConfig(
    baseUri: String,
    apiKey: String
)

trait MarketDataClient[F[_]] extends HttpClient[F]:
  def timeSeriesData(currencyPair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData]

object MarketDataClient:
  def alphaVantange[F[_]: Logger: Temporal](
      config: MarketDataClientConfig,
      backend: SttpBackend[F, Any]
  ): F[MarketDataClient[F]] =
    AlphaVantageClient.make[F](config, backend)
