package currexx.clients.broker.xtb

import cats.effect.Async
import currexx.clients.HttpClient
import currexx.clients.broker.BrokerParameters
import currexx.domain.market.{CurrencyPair, TradeOrder}
import org.typelevel.log4cats.Logger
import sttp.client3.SttpBackend

import scala.concurrent.duration.*

private[clients] trait XtbClient[F[_]] extends HttpClient[F]:
  def submit(params: BrokerParameters.Xtb, pair: CurrencyPair, order: TradeOrder): F[Unit]

final private class LiveXtbClient[F[_]](
    private val config: XtbConfig,
    override protected val backend: SttpBackend[F, Any]
)(using
    F: Async[F],
    logger: Logger[F]
) extends XtbClient[F]:

  override protected val name: String                                   = "xtb"
  override protected val delayBetweenConnectionFailures: FiniteDuration = 5.seconds

  override def submit(params: BrokerParameters.Xtb, pair: CurrencyPair, order: TradeOrder): F[Unit] = ???
