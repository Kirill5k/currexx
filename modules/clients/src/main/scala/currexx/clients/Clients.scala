package currexx.clients

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.broker.BrokerClient
import currexx.clients.broker.oanda.{OandaConfig, OandaClient}
import currexx.clients.broker.xtb.{XtbClient, XtbConfig}
import currexx.clients.data.MarketDataClient
import currexx.clients.data.alphavantage.{AlphaVantageClient, AlphaVantageConfig}
import currexx.clients.data.twelvedata.{TwelveDataClient, TwelveDataConfig}
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.WebSocketStreamBackend

final case class ClientsConfig(
    alphaVantage: AlphaVantageConfig,
    twelveData: TwelveDataConfig,
    xtb: XtbConfig,
    oanda: OandaConfig
)

final class Clients[F[_]] private (
    val marketData: MarketDataClient[F],
    val broker: BrokerClient[F]
)

object Clients:
  def make[F[_]: {Async, Logger}](
      config: ClientsConfig,
      fs2Backend: WebSocketStreamBackend[F, Fs2Streams[F]]
  ): F[Clients[F]] =
    for
      alphavantage <- AlphaVantageClient.make[F](config.alphaVantage, fs2Backend)
      twelvedata   <- TwelveDataClient.make(config.twelveData, fs2Backend)
      xtb          <- XtbClient.make[F](config.xtb, fs2Backend)
      oanda        <- OandaClient.make[F](config.oanda, fs2Backend)
      broker       <- BrokerClient.make[F](xtb, oanda)
      data         <- MarketDataClient.make[F](alphavantage, twelvedata)
    yield Clients[F](data, broker)
