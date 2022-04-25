package currexx.clients

import cats.effect.Async
import cats.syntax.functor.*
import currexx.clients.data.MarketDataClient
import currexx.clients.data.alphavantage.AlphaVantageClient
import org.typelevel.log4cats.Logger
import sttp.client3.SttpBackend

final case class ClientConfig(
    baseUri: String,
    apiKey: Option[String]
)

final case class ClientsConfig(
    alphaVantage: ClientConfig
)

final class Clients[F[_]] private (
    private val alphaVantage: MarketDataClient[F]
):
  def marketData: MarketDataClient[F] = alphaVantage

object Clients:
  def make[F[_]: Async: Logger](config: ClientsConfig, backend: SttpBackend[F, Any]): F[Clients[F]] =
    AlphaVantageClient.make[F](config.alphaVantage, backend).map(av => Clients[F](av))