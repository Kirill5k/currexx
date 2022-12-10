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
import currexx.domain.time.Clock
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.time.Instant

final private class SignalController[F[_]](
    private val service: SignalService[F]
)(using
    F: Async[F],
    clock: Clock[F]
) extends Controller[F] {
  import SignalController.*

  private def submitSignal(using auth: Authenticator[F]) =
    submitSignalEndpoint.withAuthenticatedSession
      .serverLogic { session => req =>
        for
          time <- clock.currentTime
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

  def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        submitSignal,
        getAllSignals
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

  private val basePath = "signals"

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

  def make[F[_]: Async: Clock](service: SignalService[F]): F[Controller[F]] =
    Monad[F].pure(SignalController[F](service))
}
