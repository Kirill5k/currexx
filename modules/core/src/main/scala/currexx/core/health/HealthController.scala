package currexx.core.health

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirJson, TapirSchema}
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*
import io.circe.Codec

import java.time.Instant
import org.http4s.HttpRoutes
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.net.InetAddress

final class HealthController[F[_]: Async](
    private val service: String,
    private val startupTime: Instant,
    private val ipAddress: String,
    private val appVersion: Option[String]
)(using
    clock: Clock[F]
) extends Controller[F] {

  private val statusEndpoint: ServerEndpoint[Fs2Streams[F], F] =
    HealthController.statusEndpoint
      .serverLogicSuccess { req =>
        clock
          .durationBetweenNowAnd(startupTime)
          .map { uptime =>
            HealthController.AppStatus(
              service,
              startupTime,
              uptime.toReadableString,
              appVersion,
              ipAddress
            )
          }
      }

  def routes(using auth: Authenticator[F]): HttpRoutes[F] = Http4sServerInterpreter[F]().toRoutes(statusEndpoint)
}

object HealthController extends TapirJson with TapirSchema {

  final case class AppStatus(
      service: String,
      startupTime: Instant,
      upTime: String,
      appVersion: Option[String],
      serverIpAddress: String
  ) derives Codec.AsObject

  val statusEndpoint = infallibleEndpoint.get
    .in("health" / "status")
    .out(jsonBody[AppStatus])

  def make[F[_]](using F: Async[F], C: Clock[F]): F[Controller[F]] =
    for
      now     <- C.now
      ip      <- F.blocking(InetAddress.getLocalHost.getHostAddress)
      version <- F.delay(sys.env.get("VERSION"))
    yield HealthController[F]("currexx-core", now, ip, version)
}
