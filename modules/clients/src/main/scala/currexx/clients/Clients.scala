package currexx.clients

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.broker.BrokerClient
import currexx.clients.broker.oanda.{OandaBrokerClient, OandaBrokerConfig}
import currexx.clients.broker.xtb.{XtbClient, XtbConfig}
import currexx.clients.data.MarketDataClient
import currexx.clients.data.alphavantage.AlphaVantageConfig
import currexx.clients.data.twelvedata.{TwelveDataClient, TwelveDataConfig}
import currexx.clients.data.oanda.{OandaDataClient, OandaDataConfig}
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.WebSocketStreamBackend

import scala.concurrent.duration.*

final case class ClientsConfig(
    alphaVantage: AlphaVantageConfig,
    twelveData: TwelveDataConfig,
    xtb: XtbConfig,
    oandaBroker: OandaBrokerConfig,
    oandaData: OandaDataConfig
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
      twelvedata  <- TwelveDataClient.make(config.twelveData, fs2Backend, delayBetweenClientFailures = 1.minute)
      oandadata   <- OandaDataClient.make[F](config.oandaData, fs2Backend)
      xtb         <- XtbClient.make[F](config.xtb, fs2Backend)
      oandabroker <- OandaBrokerClient.make[F](config.oandaBroker, fs2Backend)
      broker      <- BrokerClient.make[F](xtb, oandabroker)
      data        <- MarketDataClient.make[F](twelvedata, oandadata)
    yield Clients[F](data, broker)
