package currexx.clients.broker.vindaloo

import cats.Monad
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
  def submit(params: BrokerParameters.Vindaloo, cp: CurrencyPair, order: TradeOrder): F[Unit]
  def getCurrentOrder(params: BrokerParameters.Vindaloo, cp: CurrencyPair): F[Option[OpenedTradeOrder]]

final private class LiveVindalooClient[F[_]](
    private val config: VindalooConfig,
    override protected val backend: SttpBackend[F, Any]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends VindalooClient[F] {

  override protected val name: String                                   = "vindaloo"
  override protected val delayBetweenConnectionFailures: FiniteDuration = 5.seconds

  override def submit(params: BrokerParameters.Vindaloo, cp: CurrencyPair, order: TradeOrder): F[Unit] =
    logger.info(s"Submitting $cp order $order to Vindaloo(${params.externalId})") >> {
      order match
        case enter: TradeOrder.Enter => enterMarketOrder(params.externalId, cp, enter)
        case TradeOrder.Exit         => exitMarketOrder(params.externalId, cp)
    }

  private def enterMarketOrder(externalId: String, currencyPair: CurrencyPair, order: TradeOrder.Enter): F[Unit] = {
    val stopLoss   = order.stopLoss.getOrElse(0)
    val trailingSL = order.trailingStopLoss.getOrElse(0)
    val takeProfit = order.takeProfit.getOrElse(0)
    val pos        = order.position.toString.toLowerCase
    sendRequest(uri"${config.baseUri}/$externalId/$stopLoss/$trailingSL/$takeProfit/$pos/$currencyPair/${order.volume}")
  }

  private def exitMarketOrder(externalId: String, currencyPair: CurrencyPair): F[Unit] =
    sendRequest(uri"${config.baseUri}/close/$externalId/$currencyPair")

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

  override def getCurrentOrder(params: BrokerParameters.Vindaloo, cp: CurrencyPair): F[Option[OpenedTradeOrder]] = F.pure(None)
}

object VindalooClient:
  def make[F[_]: Temporal: Logger](
      config: VindalooConfig,
      backend: SttpBackend[F, Any]
  ): F[VindalooClient[F]] =
    Monad[F].pure(LiveVindalooClient(config, backend))
