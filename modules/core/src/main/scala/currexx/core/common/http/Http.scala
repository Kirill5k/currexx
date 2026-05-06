package currexx.core.common.http

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.semigroupk.*
import currexx.core.auth.{Auth, Authenticator}
import currexx.core.health.Health
import currexx.core.market.Markets
import currexx.core.signal.Signals
import currexx.core.monitor.Monitors
import currexx.core.settings.Settings
import currexx.core.trade.Trades
import org.http4s.*
import org.http4s.server.Router
import org.http4s.server.middleware.*

import scala.concurrent.duration.*

final class Http[F[_]: Async] private (
    private val health: Health[F],
    private val auth: Auth[F],
    private val signals: Signals[F],
    private val monitors: Monitors[F],
    private val markets: Markets[F],
    private val trades: Trades[F],
    private val settings: Settings[F],
    private val redactor: SensitiveDataRedactor[F]
) {

  private val apiRoutes: HttpRoutes[F] = {
    given Authenticator[F] = auth.authenticator
    val routes             = auth.controller.routes <+>
      signals.controller.routes <+>
      monitors.controller.routes <+>
      markets.controller.routes <+>
      trades.controller.routes <+>
      settings.controller.routes
    Router("/api" -> routes)
  }

  private val healthRoutes: HttpRoutes[F] = {
    given Authenticator[F] = auth.authenticator
    Router("/" -> health.controller.routes)
  }

  private val middleware: HttpRoutes[F] => HttpRoutes[F] = { (http: HttpRoutes[F]) => AutoSlash(http) }
    .andThen((http: HttpRoutes[F]) => CORS.policy.withAllowOriginAll.withAllowCredentials(false).apply(http))
    .andThen((http: HttpRoutes[F]) => Timeout(60.seconds)(http))

  private val loggers: HttpRoutes[F] => HttpRoutes[F] = { (http: HttpRoutes[F]) =>
    RequestLogger.httpRoutes(true, true, logAction = Some(redactor.log))(http)
  }.andThen((http: HttpRoutes[F]) => ResponseLogger.httpRoutes(true, true, logAction = Some(redactor.log))(http))

  val app: HttpRoutes[F] = loggers(middleware(apiRoutes)) <+> healthRoutes
}

object Http:
  def make[F[_]: Async](
      health: Health[F],
      auth: Auth[F],
      signals: Signals[F],
      monitors: Monitors[F],
      markets: Markets[F],
      trades: Trades[F],
      settings: Settings[F]
  ): F[Http[F]] =
    Http[F](
      health = health,
      auth = auth,
      signals = signals,
      monitors = monitors,
      markets = markets,
      trades = trades,
      settings = settings,
      redactor = SensitiveDataRedactor[F](Set("password", "currentPassword", "newPassword", "apiKey"))
    ).pure[F]
