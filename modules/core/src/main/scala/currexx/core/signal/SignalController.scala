package currexx.core.signal

import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.effect.Async
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirJson, TapirSchema}
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.signal.{Condition, Indicator}
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.time.Instant

final private class SignalController[F[_]](
    private val service: SignalService[F]
)(using
    F: Async[F]
) extends Controller[F] {
  import SignalController.*

  private def getAllSignals(using auth: Authenticator[F]) =
    getAllSignalsEndpoint.withAuthenticatedSession
      .serverLogic { session => sp =>
        validateDateRange(sp) >>
          service
            .getAll(session.userId, sp)
            .mapResponse(_.map(SignalView.from))
      }

  def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(getAllSignals)
    )
}

object SignalController extends TapirSchema with TapirJson {

  final case class SignalView(
      interval: Interval,
      currencyPair: CurrencyPair,
      condition: Condition,
      triggeredBy: Indicator,
      time: Instant
  ) derives Codec.AsObject

  object SignalView:
    def from(signal: Signal): SignalView =
      SignalView(
        interval = signal.interval,
        currencyPair = signal.currencyPair,
        condition = signal.condition,
        triggeredBy = signal.triggeredBy,
        time = signal.time
      )

  private val basePath = "signals"

  val getAllSignalsEndpoint = Controller.securedEndpoint.get
    .in(basePath)
    .in(Controller.searchParams)
    .out(jsonBody[List[SignalView]])
    .description("Retrieve stored signals")

  def make[F[_]: Async](service: SignalService[F]): F[Controller[F]] =
    SignalController[F](service).pure[F]
}
