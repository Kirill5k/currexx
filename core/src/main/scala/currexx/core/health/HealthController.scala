package currexx.core.health

import cats.effect.Async
import cats.effect.Temporal
import cats.syntax.functor.*
import currexx.core.auth.Authenticator
import currexx.core.common.http.Controller
import io.circe.Codec

import java.time.Instant
import org.http4s.HttpRoutes
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter

final class HealthController[F[_]: Async](
    private val startupTime: Instant
) extends Controller[F] {

  private val statusEndpoint: ServerEndpoint[Any, F] = infallibleEndpoint.get
    .in("health" / "status")
    .out(jsonBody[HealthController.AppStatus])
    .serverLogicPure(_ => Right(HealthController.AppStatus(startupTime)))

  def routes(using auth: Authenticator[F]): HttpRoutes[F] = Http4sServerInterpreter[F]().toRoutes(statusEndpoint)
}

object HealthController:

  final case class AppStatus(startupTime: Instant) derives Codec.AsObject

  def make[F[_]: Async]: F[Controller[F]] =
    Temporal[F].realTimeInstant
      .map(ts => HealthController[F](ts))
