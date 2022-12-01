package currexx.core.settings

import cats.Monad
import cats.effect.Async
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirJson, TapirSchema}
import currexx.domain.user.UserId
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class SettingsController[F[_]](
    private val service: SettingsService[F]
)(using
    F: Async[F]
) extends Controller[F] {
  import SettingsController.*

  private def getSettings(using auth: Authenticator[F]) =
    getGlobalSettingsEndpoint.withAuthenticatedSession
      .serverLogic { session => _ =>
        service
          .get(session.userId)
          .mapResponse(s => GlobalSettingsView(s.signal, s.trade))
      }

  private def updateSettings(using auth: Authenticator[F]) =
    updateGlobalSettingsEndpoint.withAuthenticatedSession
      .serverLogic(session => req => service.update(req.toDomain(session.userId)).voidResponse)

  override def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getSettings,
        updateSettings
      )
    )
}

object SettingsController extends TapirSchema with TapirJson {

  final case class GlobalSettingsView(
      signal: Option[SignalParameters],
      trade: Option[TradeParameters]
  ) derives Codec.AsObject:
    def toDomain(uid: UserId): GlobalSettings = GlobalSettings(uid, signal, trade)

  private val basePath = "settings"

  val getGlobalSettingsEndpoint = Controller.securedEndpoint.get
    .in(basePath)
    .out(jsonBody[GlobalSettingsView])
    .description("Retrieve global settings")

  val updateGlobalSettingsEndpoint = Controller.securedEndpoint.put
    .in(basePath)
    .in(jsonBody[GlobalSettingsView])
    .out(statusCode(StatusCode.NoContent))
    .description("Update global settings")

  def make[F[_]: Async](monitorService: SettingsService[F]): F[Controller[F]] =
    Monad[F].pure(SettingsController[F](monitorService))
}
