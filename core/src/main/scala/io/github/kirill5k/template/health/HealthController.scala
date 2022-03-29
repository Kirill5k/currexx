package io.github.kirill5k.template.health

import cats.effect.Async
import cats.effect.Ref
import cats.effect.Temporal
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.Codec
import io.github.kirill5k.template.common.http.Controller
import java.time.Instant
import org.http4s.HttpRoutes
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter

final class HealthController[F[_]: Async](
    private val startupTime: Ref[F, Instant]
) extends Controller[F] {

  implicit val statusSchema: Schema[HealthController.AppStatus] = Schema.string

  private val statusEndpoint: ServerEndpoint[Any, F] = infallibleEndpoint.get
    .in("health" / "status")
    .out(jsonBody[HealthController.AppStatus])
    .serverLogicSuccess(req => startupTime.get.map(t => HealthController.AppStatus(t)))

  def routes: HttpRoutes[F] = Http4sServerInterpreter[F]().toRoutes(statusEndpoint)
}

object HealthController:

  final case class AppStatus(startupTime: Instant) derives Codec.AsObject

  def make[F[_]: Async]: F[Controller[F]] =
    Temporal[F].realTimeInstant
      .flatMap(ts => Ref.of(ts))
      .map(ref => HealthController[F](ref))
