package currexx.core.common.http

import cats.Monad
import cats.effect.Async
import cats.implicits.*
import currexx.core.auth.jwt.BearerToken
import currexx.core.auth.{Auth, Authenticator}
import currexx.core.health.Health
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.server.Router
import org.http4s.server.middleware.*

import scala.concurrent.duration.*

final class Http[F[_]: Async] private (
    private val health: Health[F],
    private val auth: Auth[F]
) {

  private val routes: HttpRoutes[F] = {
    given Authenticator[F] = (token: BearerToken) => auth.session.authenticate(token)
    val api                = auth.controller.routes
    Router("/api" -> api, "/" -> health.controller.routes)
  }

  private val middleware: HttpRoutes[F] => HttpRoutes[F] = { (http: HttpRoutes[F]) => AutoSlash(http) }
    .andThen((http: HttpRoutes[F]) => CORS.policy.withAllowOriginAll.withAllowCredentials(false).apply(http))
    .andThen((http: HttpRoutes[F]) => Timeout(60.seconds)(http))

  private val loggers: HttpApp[F] => HttpApp[F] = { (http: HttpApp[F]) => RequestLogger.httpApp(true, true)(http) }
    .andThen((http: HttpApp[F]) => ResponseLogger.httpApp(true, true)(http))

  val app: HttpApp[F] = loggers(middleware(routes).orNotFound)
}

object Http:
  def make[F[_]: Async](
      health: Health[F],
      auth: Auth[F]
  ): F[Http[F]] = Monad[F].pure(new Http[F](health, auth))
