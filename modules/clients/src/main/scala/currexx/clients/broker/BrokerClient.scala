package currexx.clients.broker

import cats.Monad
import cats.data.NonEmptyList
import currexx.clients.broker.vindaloo.VindalooClient
import currexx.clients.broker.xtb.XtbClient
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, TradeOrder}

trait BrokerClient[F[_]]:
  def submit(parameters: BrokerParameters, order: TradeOrder): F[Unit]
  def find(parameters: BrokerParameters, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]]

final private class LiveBrokerClient[F[_]](
    private val vindalooClient: VindalooClient[F],
    private val xtbClient: XtbClient[F]
) extends BrokerClient[F]:
  override def find(parameters: BrokerParameters, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] =
    parameters match
      case params: BrokerParameters.Vindaloo => vindalooClient.getCurrentOrders(params, cps)
      case params: BrokerParameters.Xtb      => xtbClient.getCurrentOrders(params, cps)

  override def submit(parameters: BrokerParameters, order: TradeOrder): F[Unit] =
    parameters match
      case params: BrokerParameters.Vindaloo => vindalooClient.submit(params, order)
      case params: BrokerParameters.Xtb      => xtbClient.submit(params, order)

object BrokerClient:
  def make[F[_]: Monad](
      vindalooClient: VindalooClient[F],
      xtbClient: XtbClient[F]
  ): F[BrokerClient[F]] =
    Monad[F].pure(LiveBrokerClient[F](vindalooClient, xtbClient))
