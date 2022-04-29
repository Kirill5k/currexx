package currexx.core.market

import cats.Monad
import cats.effect.Async
import cats.syntax.flatMap.*
import currexx.clients.broker.BrokerParameters
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirCodecs, TapirJson, TapirSchema}
import currexx.domain.errors.AppError
import currexx.domain.market.{MarketOrder, PriceRange}
import currexx.domain.user.UserId
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.StatusCode
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.*

import java.time.Instant

final private class MarketController[F[_]](
    private val service: MarketService[F]
)(using
    F: Async[F]
) extends Controller[F] {
  import MarketController.*
  
  private def getMarketState(using auth: Authenticator[F]) =
    getMarketStateEndpoint.withAuthenticatedSession
      .serverLogic { session => _ =>
        service
          .getState(session.userId)
          .mapResponse(_.map(s => s.currencyPair.toString -> MarketStateView.from(s)).toMap)
      }

  def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getMarketState
      )
    )
}

object MarketController extends TapirSchema with TapirJson with TapirCodecs {

  final case class MarketStateView(
      currentPosition: Option[MarketOrder.Position],
      latestPrice: Option[PriceRange],
      lastUpdatedAt: Option[Instant]
  ) derives Codec.AsObject

  object MarketStateView:
    def from(ms: MarketState): MarketStateView = MarketStateView(ms.currentPosition, ms.latestPrice, ms.lastUpdatedAt)

  private val basePath     = "market"
  private val statePath    = basePath / "state"

  val getMarketStateEndpoint = Controller.securedEndpoint.get
    .in(statePath)
    .out(jsonBody[Map[String, MarketStateView]])
    .description("Retrieve latest state of the traded currencies")

  def make[F[_]: Async](service: MarketService[F]): F[Controller[F]] =
    Monad[F].pure(MarketController[F](service))
}
