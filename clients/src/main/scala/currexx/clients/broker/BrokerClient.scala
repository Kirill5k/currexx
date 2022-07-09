package currexx.clients.broker

import cats.Monad
import cats.effect.Async
import currexx.clients.HttpClient
import currexx.clients.broker.vindaloo.VindalooClient
import currexx.clients.broker.xtb.XtbClient
import currexx.domain.market.{CurrencyPair, TradeOrder}

trait BrokerClient[F[_]]:
  def submit(pair: CurrencyPair, parameters: BrokerParameters, order: TradeOrder): F[Unit]

final private class LiveBrokerClient[F[_]](
    private val vindalooClient: VindalooClient[F],
    private val xtbClient: XtbClient[F]
) extends BrokerClient[F]:
  override def submit(pair: CurrencyPair, parameters: BrokerParameters, order: TradeOrder): F[Unit] =
    parameters match
      case params: BrokerParameters.Vindaloo => vindalooClient.submit(params, pair, order)
      case params: BrokerParameters.Xtb      => xtbClient.submit(params, pair, order)

object BrokerClient:
  def make[F[_]: Monad](
      vindalooClient: VindalooClient[F],
      xtbClient: XtbClient[F]
  ): F[BrokerClient[F]] =
    Monad[F].pure(LiveBrokerClient[F](vindalooClient, xtbClient))
