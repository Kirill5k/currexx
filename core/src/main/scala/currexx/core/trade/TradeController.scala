package currexx.core.trade

import cats.Monad
import cats.effect.Async
import currexx.clients.broker.BrokerParameters
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirCodecs, TapirJson, TapirSchema}
import currexx.domain.user.UserId
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class TradeController[F[_]](
    private val service: TradeService[F]
)(using
    F: Async[F]
) extends Controller[F] {
  import TradeController.*

  private def getTradeSettings(using auth: Authenticator[F]) =
    getTradeSettingsEndpoint.withAuthenticatedSession
      .serverLogic { session => _ =>
        service
          .getSettings(session.userId)
          .mapResponse(s => TradeSettingsView(s.broker, s.trading))
      }

  private def updateTradeSettings(using auth: Authenticator[F]) =
    updateTradeSettingsEndpoint.withAuthenticatedSession
      .serverLogic { session => settings =>
        service
          .updateSettings(settings.toDomain(session.userId))
          .voidResponse
      }

  def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getTradeSettings,
        updateTradeSettings
      )
    )
}

object TradeController extends TapirSchema with TapirJson with TapirCodecs {

  final case class TradeSettingsView(
      broker: BrokerParameters,
      trading: TradingParameters
  ) derives Codec.AsObject:
    def toDomain(userId: UserId): TradeSettings = TradeSettings(userId, broker, trading)

  private val basePath     = "trade"
  private val settingsPath = basePath / "settings"

  val getTradeSettingsEndpoint = Controller.securedEndpoint.get
    .in(settingsPath)
    .out(jsonBody[TradeSettingsView])
    .description("Retrieve settings for broker and trading")

  val updateTradeSettingsEndpoint = Controller.securedEndpoint.put
    .in(settingsPath)
    .in(jsonBody[TradeSettingsView])
    .out(statusCode(StatusCode.NoContent))
    .description("Update settings for broker and trading")

  def make[F[_]: Async](service: TradeService[F]): F[Controller[F]] =
    Monad[F].pure(TradeController[F](service))
}