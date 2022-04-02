package currexx.core.signal

import cats.Monad
import cats.effect.Async
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirJson, TapirSchema}
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.time.Instant

final private class SignalController[F[_]](
    private val service: SignalService[F]
)(using
    F: Async[F]
) extends Controller[F] {

  def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
      )
    )
}

object SignalController extends TapirSchema with TapirJson {

  final case class SubmitSignalRequest(
      currencyPair: CurrencyPair,
      indicator: Indicator
  ) derives Codec.AsObject

  final case class SignalView(
      currencyPair: CurrencyPair,
      indicator: Indicator,
      time: Instant
  ) derives Codec.AsObject

  object SignalView {
    def from(signal: Signal): SignalView =
      SignalView(signal.currencyPair, signal.indicator, signal.time)
  }

  private val basePath = "signals"

  val submitSignalEndpoint = Controller.securedEndpoint.post
    .in(basePath)
    .in(jsonBody[SubmitSignalRequest])
    .out(statusCode(StatusCode.NoContent))
    .description("Submit signal triggered by indicator")

  val getAllSignalsEndpoint = Controller.securedEndpoint.get
    .in(basePath)
    .out(jsonBody[List[SignalView]])
    .description("Retrieve all submitted signals")

  def make[F[_]: Async](service: SignalService[F]): F[Controller[F]] =
    Monad[F].pure(SignalController[F](service))
}
