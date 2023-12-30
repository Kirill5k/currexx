package currexx.clients.broker.vindaloo

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import currexx.clients.HttpClient
import currexx.clients.broker.BrokerParameters
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, TradeOrder}
import org.typelevel.log4cats.Logger
import sttp.client3.*
import sttp.model.Uri

import scala.concurrent.duration.*

private[clients] trait VindalooClient[F[_]] extends HttpClient[F]:
  def submit(params: BrokerParameters.Vindaloo, order: TradeOrder): F[Unit]
  def getCurrentOrders(params: BrokerParameters.Vindaloo, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]]

final private class LiveVindalooClient[F[_]](
    private val config: VindalooConfig,
    override protected val backend: SttpBackend[F, Any]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends VindalooClient[F] {

  override protected val name: String                                   = "vindaloo"
  override protected val delayBetweenConnectionFailures: FiniteDuration = 5.seconds

  override def submit(params: BrokerParameters.Vindaloo, order: TradeOrder): F[Unit] =
    logger.info(s"Submitting order $order to Vindaloo(${params.externalId})") >> {
      order match
        case enter: TradeOrder.Enter => enterMarketOrder(params.externalId, enter)
        case exit: TradeOrder.Exit   => exitMarketOrder(params.externalId, exit)
    }

  private def enterMarketOrder(externalId: String, order: TradeOrder.Enter): F[Unit] = {
    val stopLoss   = 0
    val trailingSL = 0
    val takeProfit = 0
    val pos        = order.position.toString.toLowerCase
    sendRequest(uri"${config.baseUri}/$externalId/$stopLoss/$trailingSL/$takeProfit/$pos/${order.currencyPair}/${order.volume}")
  }

  private def exitMarketOrder(externalId: String, order: TradeOrder.Exit): F[Unit] =
    sendRequest(uri"${config.baseUri}/close/$externalId/${order.currencyPair}")

  private def sendRequest(uri: Uri): F[Unit] =
    dispatch(basicRequest.post(uri))
      .flatMap { r =>
        r.body match {
          case Right(_) => F.unit
          case Left(error) =>
            logger.error(s"$name-client/error-${r.code.code}\n$error") *>
              F.sleep(delayBetweenConnectionFailures) *> sendRequest(uri)
        }
      }

  override def getCurrentOrders(params: BrokerParameters.Vindaloo, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] =
    F.pure(Nil)
}

object VindalooClient:
  def make[F[_]: Temporal: Logger](
      config: VindalooConfig,
      backend: SttpBackend[F, Any]
  ): F[VindalooClient[F]] =
    Monad[F].pure(LiveVindalooClient(config, backend))
