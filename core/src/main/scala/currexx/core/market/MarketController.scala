package currexx.core.market

import cats.Monad
import cats.effect.Async
import cats.syntax.flatMap.*
import currexx.clients.broker.BrokerParameters
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirCodecs, TapirJson, TapirSchema}
import currexx.core.signal.SignalController.{SignalSettingsView, jsonBody, settingsPath}
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

  private def getMarketSettings(using auth: Authenticator[F]) =
    getMarketSettingsEndpoint.withAuthenticatedSession
      .serverLogic { session => _ =>
        service
          .getSettings(session.userId)
          .mapResponse(s => MarketSettingsView(s.broker, s.trading))
      }

  private def updateMarketSettings(using auth: Authenticator[F]) =
    updateMarketSettingsEndpoint.withAuthenticatedSession
      .serverLogic { session => settings =>
        service
          .updateSettings(settings.toDomain(session.userId))
          .voidResponse
      }

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
        getMarketSettings,
        updateMarketSettings,
        getMarketState
      )
    )
}

object MarketController extends TapirSchema with TapirJson with TapirCodecs {

  final case class MarketSettingsView(
      broker: BrokerParameters,
      trading: TradingParameters
  ) derives Codec.AsObject:
    def toDomain(userId: UserId): MarketSettings = MarketSettings(userId, broker, trading)

  final case class MarketStateView(
      currentPosition: Option[MarketOrder.Position],
      latestPrice: Option[PriceRange],
      lastUpdatedAt: Option[Instant]
  ) derives Codec.AsObject

  object MarketStateView:
    def from(ms: MarketState): MarketStateView = MarketStateView(ms.currentPosition, ms.latestPrice, ms.lastUpdatedAt)

  private val basePath     = "market"
  private val settingsPath = basePath / "settings"
  private val statePath    = basePath / "state"

  val getMarketSettingsEndpoint = Controller.securedEndpoint.get
    .in(settingsPath)
    .out(jsonBody[MarketSettingsView])
    .description("Retrieve settings for broker and trading")

  val updateMarketSettingsEndpoint = Controller.securedEndpoint.put
    .in(settingsPath)
    .in(jsonBody[MarketSettingsView])
    .out(statusCode(StatusCode.NoContent))
    .description("Update settings for broker and trading")

  val getMarketStateEndpoint = Controller.securedEndpoint.get
    .in(statePath)
    .out(jsonBody[Map[String, MarketStateView]])
    .description("Retrieve latest state of the traded currencies")

  def make[F[_]: Async](service: MarketService[F]): F[Controller[F]] =
    Monad[F].pure(MarketController[F](service))
}
