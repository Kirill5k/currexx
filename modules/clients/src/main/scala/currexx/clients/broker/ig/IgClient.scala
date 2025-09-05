package currexx.clients.broker.ig

import cats.data.NonEmptyList
import cats.effect.Async
import currexx.clients.Fs2HttpClient
import currexx.clients.broker.BrokerParameters
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, TradeOrder}
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.WebSocketStreamBackend

private[clients] trait IgClient[F[_]] extends Fs2HttpClient[F]:
  def submit(params: BrokerParameters.Ig, order: TradeOrder): F[Unit]
  def getCurrentOrders(params: BrokerParameters.Ig, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]]

final private class LiveXtbClient[F[_]](
    override protected val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val config: IgConfig
)(using
    F: Async[F],
    logger: Logger[F]
) extends IgClient[F] {
  
  override protected val name: String = "ig"
  
  override def submit(params: BrokerParameters.Ig, order: TradeOrder): F[Unit] = ???

  override def getCurrentOrders(params: BrokerParameters.Ig, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] = ???
}
