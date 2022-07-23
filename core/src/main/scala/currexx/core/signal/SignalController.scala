package currexx.core.signal

import cats.Monad
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.Async
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirCodecs, TapirJson, TapirSchema}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import currexx.domain.market.{Condition, CurrencyPair, Indicator}
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import squants.market.Currency

import java.time.Instant

final private class SignalController[F[_]](
    private val service: SignalService[F]
)(using
    F: Async[F]
) extends Controller[F] {
  import SignalController.*

  private def submitSignal(using auth: Authenticator[F]) =
    submitSignalEndpoint.withAuthenticatedSession
      .serverLogic { session => req =>
        for
          time <- F.realTimeInstant
          signal = Signal(session.userId, req.currencyPair, req.condition, req.triggeredBy, time)
          res <- service.submit(signal).voidResponse
        yield res
      }

  private def getAllSignals(using auth: Authenticator[F]) =
    getAllSignalsEndpoint.withAuthenticatedSession
      .serverLogic { session => sp =>
        (sp.from, sp.to).mapN((f, t) => F.raiseWhen(f.isAfter(t))(AppError.InvalidDateRange)).getOrElse(F.unit) >>
          service
            .getAll(session.userId, sp)
            .mapResponse(_.map(SignalView.from))
      }

  private def getSignalSettings(using auth: Authenticator[F]) =
    getSignalSettingsEndpoint.withAuthenticatedSession
      .serverLogic { session => _ =>
        service
          .getSettings(session.userId)
          .mapResponse(s => SignalSettingsView(s.triggerFrequency, s.indicators))
      }

  private def updateSignalSettings(using auth: Authenticator[F]) =
    updateSignalSettingsEndpoint.withAuthenticatedSession
      .serverLogic { session => settings =>
        service
          .updateSettings(settings.toDomain(session.userId))
          .voidResponse
      }

  def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        submitSignal,
        getAllSignals,
        getSignalSettings,
        updateSignalSettings
      )
    )
}

object SignalController extends TapirSchema with TapirJson {

  final case class SubmitSignalRequest(
      currencyPair: CurrencyPair,
      condition: Condition,
      triggeredBy: Indicator
  ) derives Codec.AsObject

  final case class SignalView(
      currencyPair: CurrencyPair,
      condition: Condition,
      triggeredBy: Indicator,
      time: Instant
  ) derives Codec.AsObject

  object SignalView:
    def from(signal: Signal): SignalView =
      SignalView(signal.currencyPair, signal.condition, signal.triggeredBy, signal.time)

  final case class SignalSettingsView(
      triggerFrequency: TriggerFrequency,
      indicators: List[Indicator]
  ) derives Codec.AsObject:
    def toDomain(userId: UserId): SignalSettings = SignalSettings(userId, triggerFrequency, indicators)

  private val basePath     = "signals"
  private val settingsPath = basePath / "settings"

  val submitSignalEndpoint = Controller.securedEndpoint.post
    .in(basePath)
    .in(jsonBody[SubmitSignalRequest])
    .out(statusCode(StatusCode.NoContent))
    .description("Submit signal triggered by indicator")

  val getAllSignalsEndpoint = Controller.securedEndpoint.get
    .in(basePath)
    .in(Controller.searchParams)
    .out(jsonBody[List[SignalView]])
    .description("Retrieve all submitted signals")

  val getSignalSettingsEndpoint = Controller.securedEndpoint.get
    .in(settingsPath)
    .out(jsonBody[SignalSettingsView])
    .description("Retrieve settings for active indicators and emitted signals")

  val updateSignalSettingsEndpoint = Controller.securedEndpoint.put
    .in(settingsPath)
    .in(jsonBody[SignalSettingsView])
    .out(statusCode(StatusCode.NoContent))
    .description("Update settings for active indicators")

  def make[F[_]: Async](service: SignalService[F]): F[Controller[F]] =
    Monad[F].pure(SignalController[F](service))
}
