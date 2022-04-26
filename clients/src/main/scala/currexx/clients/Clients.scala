package currexx.clients

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.broker.BrokerClient
import currexx.clients.broker.vindaloo.VindalooClient
import currexx.clients.data.MarketDataClient
import currexx.clients.data.alphavantage.AlphaVantageClient
import org.typelevel.log4cats.Logger
import sttp.client3.SttpBackend

final case class ClientConfig(
    baseUri: String,
    apiKey: Option[String]
)

final case class ClientsConfig(
    alphaVantage: ClientConfig,
    vindaloo: ClientConfig
)

final class Clients[F[_]] private (
    val marketData: MarketDataClient[F],
    val broker: BrokerClient[F]
)

object Clients:
  def make[F[_]: Async: Logger](config: ClientsConfig, backend: SttpBackend[F, Any]): F[Clients[F]] =
    for
      alphavantage <- AlphaVantageClient.make[F](config.alphaVantage, backend)
      vindaloo     <- VindalooClient.make[F](config.vindaloo, backend)
      broker       <- BrokerClient.make[F](vindaloo)
      data         <- MarketDataClient.make[F](alphavantage)
    yield Clients[F](data, broker)
