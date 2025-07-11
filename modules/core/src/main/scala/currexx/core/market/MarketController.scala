package currexx.core.market

import cats.Monad
import cats.effect.Async
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirCodecs, TapirJson, TapirSchema}
import currexx.domain.market.CurrencyPair
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

  private def clearMarketState(using auth: Authenticator[F]) =
    clearMarketStateEndpoint.withAuthenticatedSession
      .serverLogic { session => (closePendingOrders, dryRun, currencyPair) =>
        F.whenA(!dryRun) {
          currencyPair match
            case Some(cp) => service.clearState(session.userId, cp, closePendingOrders)
            case None     => service.clearState(session.userId, closePendingOrders)
        }.voidResponse
      }

  def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getMarketState,
        clearMarketState
      )
    )
}

object MarketController extends TapirSchema with TapirJson with TapirCodecs {

  final case class MarketStateView(
      currentPosition: Option[PositionState],
      profile: MarketProfile,
      lastUpdatedAt: Instant,
      createdAt: Instant
  ) derives Codec.AsObject

  object MarketStateView:
    def from(ms: MarketState): MarketStateView =
      MarketStateView(
        currentPosition = ms.currentPosition,
        profile = ms.profile,
        lastUpdatedAt = ms.lastUpdatedAt,
        createdAt = ms.createdAt
      )

  private val basePath  = "market"
  private val statePath = basePath / "state"

  val getMarketStateEndpoint = Controller.securedEndpoint.get
    .in(statePath)
    .out(jsonBody[Map[String, MarketStateView]])
    .description("Retrieve latest state of traded currencies")

  val clearMarketStateEndpoint = Controller.securedEndpoint.delete
    .in(statePath)
    .in(
      query[Boolean]("closePendingOrders")
        .default(true)
        .description("Close pending orders")
        .and(query[Boolean]("dryRun").default(true))
        .and(query[Option[CurrencyPair]]("currencyPair"))
    )
    .out(statusCode(StatusCode.NoContent))
    .description("Clear all states of traded currencies")

  def make[F[_]: Async](service: MarketService[F]): F[Controller[F]] =
    Monad[F].pure(MarketController[F](service))
}
