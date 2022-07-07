package currexx.clients.broker.xtb

import cats.Monad
import cats.effect.Async
import cats.syntax.functor.*
import currexx.clients.HttpClient
import currexx.clients.broker.BrokerParameters
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, TradeOrder}
import io.circe.syntax.*
import org.typelevel.log4cats.Logger
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.SttpBackend
import sttp.ws.WebSocketFrame
import sttp.client3.*
import sttp.model.Uri
import fs2.{Pipe, Stream}

import scala.concurrent.duration.*
import java.nio.charset.StandardCharsets

private[clients] trait XtbClient[F[_]] extends HttpClient[F]:
  def submit(params: BrokerParameters.Xtb, pair: CurrencyPair, order: TradeOrder): F[Unit]

final private class LiveXtbClient[F[_]](
    private val config: XtbConfig,
    override protected val backend: SttpBackend[F, Fs2Streams[F] with WebSockets]
)(using
    F: Async[F],
    logger: Logger[F]
) extends XtbClient[F] {

  override protected val name: String                                   = "xtb"
  override protected val delayBetweenConnectionFailures: FiniteDuration = 5.seconds

  override def submit(params: BrokerParameters.Xtb, pair: CurrencyPair, order: TradeOrder): F[Unit] =
    basicRequest
      .get(uri"${config.baseUri}/${if (params.demo) "demo" else "real"}")
      .response(asWebSocketStream(Fs2Streams[F])(orderPlacementPipe(params, pair, order)))
      .send(backend)
      .void

  private def orderPlacementPipe(
      params: BrokerParameters.Xtb,
      pair: CurrencyPair,
      order: TradeOrder
  ): Pipe[F, WebSocketFrame.Data[_], WebSocketFrame] = { input =>
    Stream.emit(WebSocketFrame.text(XtbRequest.login(params.userId, params.password).asJson.noSpaces)) ++
      input
        .map {
          case WebSocketFrame.Text(jsonPayload, _, _) => XtbResponse.fromJson(jsonPayload)
          case WebSocketFrame.Binary(bytes, _, _)     => XtbResponse.fromJson(new String(bytes, StandardCharsets.UTF_8))
          case _ | null                               => Right(XtbResponse.Void)
        }
        .rethrow
        .flatMap {
          case XtbResponse.Login(sessionId) =>
            Stream.emit(WebSocketFrame.text(XtbRequest.trade(sessionId, pair, order).asJson.noSpaces))
          case XtbResponse.OrderPlacement(_) =>
            Stream.emit(WebSocketFrame.close)
          case XtbResponse.Error("BE005", desc) =>
            Stream.emit(WebSocketFrame.close) ++
              Stream.eval(logger.error(s"$name-client/forbidden: $desc")).drain ++
              Stream.raiseError(AppError.AccessDenied(s"Failed to authenticate with $name: $desc"))
          case XtbResponse.Error(code, desc) =>
            Stream.emit(WebSocketFrame.close) ++
              Stream.eval(logger.error(s"$name-client/error: $desc")).drain ++
              Stream.raiseError(AppError.ClientFailure(name, s"$code - $desc"))
          case _ => Stream.empty
        }
  }
}

object XtbClient:
  def make[F[_]: Async: Logger](
      config: XtbConfig,
      backend: SttpBackend[F, Fs2Streams[F] with WebSockets]
  ): F[XtbClient[F]] =
    Monad[F].pure(LiveXtbClient(config, backend))
