package currexx.core.signal

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.Async
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirCodecs, TapirJson, TapirSchema}
import currexx.domain.market.{Condition, CurrencyPair, Indicator, IndicatorParameters}
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
          signal = Signal(session.userId, req.currencyPair, req.indicator, req.condition, time)
          res <- service.submit(signal).voidResponse
        yield res
      }

  private def getAllSignals(using auth: Authenticator[F]) =
    getAllSignalsEndpoint.withAuthenticatedSession
      .serverLogic { session => _ =>
        service
          .getAll(session.userId)
          .mapResponse(_.map(SignalView.from))
      }

  private def getSignalSettings(using auth: Authenticator[F]) =
    getSignalSettingsForCurrencyPairEndpoint.withAuthenticatedSession
      .serverLogic { session => (base, quote) =>
        service
          .getSettings(session.userId, CurrencyPair(base, quote))
          .mapResponse(_.indicators)
      }

  private def updateSignalSettings(using auth: Authenticator[F]) =
    updateSignalSettingsForCurrencyPairEndpoint.withAuthenticatedSession
      .serverLogic { session => (base, quote, indicators) =>
        service
          .update(SignalSettings(session.userId, CurrencyPair(base, quote), indicators))
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

object SignalController extends TapirSchema with TapirJson with TapirCodecs {

  final case class SubmitSignalRequest(
      currencyPair: CurrencyPair,
      indicator: Indicator,
      condition: Condition
  ) derives Codec.AsObject

  final case class SignalView(
      currencyPair: CurrencyPair,
      indicator: Indicator,
      condition: Condition,
      time: Instant
  ) derives Codec.AsObject

  object SignalView:
    def from(signal: Signal): SignalView =
      SignalView(signal.currencyPair, signal.indicator, signal.condition, signal.time)

  private val basePath                   = "signals"
  private val settingsPath               = basePath / "settings"
  private val settingsByCurrencyPairPath = settingsPath / path[Currency]("base") / path[Currency]("quote")

  val submitSignalEndpoint = Controller.securedEndpoint.post
    .in(basePath)
    .in(jsonBody[SubmitSignalRequest])
    .out(statusCode(StatusCode.NoContent))
    .description("Submit signal triggered by indicator")

  val getAllSignalsEndpoint = Controller.securedEndpoint.get
    .in(basePath)
    .out(jsonBody[List[SignalView]])
    .description("Retrieve all submitted signals")

  val getSignalSettingsForCurrencyPairEndpoint = Controller.securedEndpoint.get
    .in(settingsByCurrencyPairPath)
    .out(jsonBody[List[IndicatorParameters]])
    .description("Retrieve signal settings for a given currency pair")

  val updateSignalSettingsForCurrencyPairEndpoint = Controller.securedEndpoint.put
    .in(settingsByCurrencyPairPath)
    .in(jsonBody[List[IndicatorParameters]])
    .out(statusCode(StatusCode.NoContent))
    .description("Update signal settings for a given currency pair")

  def make[F[_]: Async](service: SignalService[F]): F[Controller[F]] =
    Monad[F].pure(SignalController[F](service))
}
