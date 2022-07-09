package currexx.clients

import cats.effect.{Async, Temporal}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.broker.BrokerClient
import currexx.clients.broker.vindaloo.{VindalooClient, VindalooConfig}
import currexx.clients.broker.xtb.{XtbClient, XtbConfig}
import currexx.clients.data.MarketDataClient
import currexx.clients.data.alphavantage.{AlphaVantageClient, AlphaVantageConfig}
import org.typelevel.log4cats.Logger
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.SttpBackend

final case class ClientsConfig(
    alphaVantage: AlphaVantageConfig,
    vindaloo: VindalooConfig,
    xtb: XtbConfig
)

final class Clients[F[_]] private (
    val marketData: MarketDataClient[F],
    val broker: BrokerClient[F]
)

object Clients:
  def make[F[_]: Async: Logger](
      config: ClientsConfig,
      backend: SttpBackend[F, Fs2Streams[F] with WebSockets]
  ): F[Clients[F]] =
    for
      alphavantage <- AlphaVantageClient.make[F](config.alphaVantage, backend)
      vindaloo     <- VindalooClient.make[F](config.vindaloo, backend)
      xtb          <- XtbClient.make[F](config.xtb, backend)
      broker       <- BrokerClient.make[F](vindaloo, xtb)
      data         <- MarketDataClient.make[F](alphavantage)
    yield Clients[F](data, broker)
