package currexx.clients.broker.vindaloo

import cats.Monad
import cats.effect.{Async, Temporal}
import currexx.clients.{ClientConfig, HttpClient}
import currexx.clients.broker.{BrokerClient, BrokerParameters}
import currexx.domain.market.Order
import org.typelevel.log4cats.Logger
import sttp.client3.SttpBackend

import scala.concurrent.duration.*

private[clients] trait VindalooClient[F[_]] extends HttpClient[F]:
  def submit(externalId: String, order: Order): F[Unit]

final private class LiveVindalooClient[F[_]](
    private val config: ClientConfig,
    override protected val backend: SttpBackend[F, Any]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends VindalooClient[F] {

  override protected val name: String                         = "vindaloo"
  override protected val delayBetweenFailures: FiniteDuration = 5.seconds

  override def submit(externalId: String, order: Order): F[Unit] =
    order match
      case enter: Order.EnterMarket => enterMarketOrder(externalId, enter)
      case exit: Order.ExitMarket   => exitMarketOrder(externalId, exit)

  private def enterMarketOrder(externalId: String, order: Order.EnterMarket): F[Unit] = ???

  private def exitMarketOrder(externalId: String, order: Order.ExitMarket): F[Unit] = ???
}

object VindalooClient:
  def make[F[_]: Temporal: Logger](
      config: ClientConfig,
      backend: SttpBackend[F, Any]
  ): F[VindalooClient[F]] =
    Monad[F].pure(LiveVindalooClient(config, backend))
