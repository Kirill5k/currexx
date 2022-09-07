package currexx.clients.broker.xtb

import cats.Monad
import cats.effect.{Async, Ref}
import cats.syntax.functor.*
import cats.syntax.flatMap.*
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
import io.circe.Encoder
import sttp.ws.WebSocketFrame.Text

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

  override def submit(params: BrokerParameters.Xtb, pair: CurrencyPair, order: TradeOrder): F[Unit] = {
    val process = order match
      case TradeOrder.Exit         => orderClosureProcess(params, pair)
      case enter: TradeOrder.Enter => orderPlacementProcess(params, pair, enter)

    basicRequest
      .response(asWebSocketStream(Fs2Streams[F])(process))
      .get(uri"${config.baseUri}/${if (params.demo) "demo" else "real"}")
      .send(backend)
      .void
  }

  private def orderPlacementProcess(
      params: BrokerParameters.Xtb,
      pair: CurrencyPair,
      order: TradeOrder.Enter
  ): Pipe[F, WebSocketFrame.Data[_], WebSocketFrame] = { input =>
    initEmptyState.flatMap { state =>
      login(params) ++
        input
          .map(parseXtbResponse)
          .rethrow
          .flatMap {
            case XtbResponse.Login(sessionId) =>
              Stream.eval(state.update(_.withSessionId(sessionId))).drain ++
                Stream.emit(XtbRequest.symbolInfo(sessionId, pair).asText)
            case XtbResponse.SymbolInfo(price) =>
              obtainSessionId(state).map(sid => XtbRequest.openTransaction(sid, pair, order, price).asText)
            case XtbResponse.OrderPlacement(_) => Stream.emit(WebSocketFrame.close)
            case error: XtbResponse.Error      => handError(error)
            case _                             => Stream.empty
          }
    }
  }

  private def orderClosureProcess(
      params: BrokerParameters.Xtb,
      cp: CurrencyPair
  ): Pipe[F, WebSocketFrame.Data[_], WebSocketFrame] = { input =>
    initEmptyState.flatMap { state =>
      login(params) ++
        input
          .map(parseXtbResponse)
          .rethrow
          .flatMap {
            case XtbResponse.Login(sessionId) =>
              Stream.eval(state.update(_.withSessionId(sessionId))).drain ++
                Stream.emit(XtbRequest.currentTrades(sessionId).asText)
            case XtbResponse.Trades(trades) =>
              obtainSessionId(state)
                .flatMap { sid =>
                  Stream
                    .emits(trades)
                    .filter(_.symbol == cp.toString)
                    .map(td => XtbRequest.closeTransaction(sid, cp, td).asText)
                }
            case XtbResponse.OrderPlacement(_) => Stream.emit(WebSocketFrame.close)
            case XtbResponse.Error("SE199", _) => obtainSessionId(state).map(sid => XtbRequest.currentTrades(sid).asText)
            case error: XtbResponse.Error      => handError(error)
            case _                             => Stream.empty
          }
    }
  }

  private val parseXtbResponse: WebSocketFrame.Data[_] => Either[AppError, XtbResponse] = {
    case WebSocketFrame.Text(jsonPayload, _, _) => XtbResponse.fromJson(jsonPayload)
    case WebSocketFrame.Binary(bytes, _, _)     => XtbResponse.fromJson(new String(bytes, StandardCharsets.UTF_8))
    case _ | null                               => Right(XtbResponse.Void)
  }

  private def login(params: BrokerParameters.Xtb): Stream[F, Text] =
    Stream.emit(XtbRequest.login(params.userId, params.password).asText)

  private def initEmptyState: Stream[F, Ref[F, XtbClient.WsState]] =
    Stream.eval(Ref.of(XtbClient.WsState(None)))

  private def obtainSessionId(state: Ref[F, XtbClient.WsState]): Stream[F, String] =
    Stream.eval(state.get.map(_.sessionId.toRight(AppError.ClientFailure(name, "no session id")))).rethrow

  private def handError(error: XtbResponse.Error): Stream[F, WebSocketFrame] =
    error match
      case XtbResponse.Error("BE005", desc) =>
        Stream.emit(WebSocketFrame.close) ++
          Stream.logError(s"$name-client/forbidden: $desc") ++
          Stream.raiseError(AppError.AccessDenied(s"Failed to authenticate with $name: $desc"))
      case XtbResponse.Error(code, desc) =>
        Stream.emit(WebSocketFrame.close) ++
          Stream.logError(s"$name-client/error: $desc") ++
          Stream.raiseError(AppError.ClientFailure(name, s"$code - $desc"))

  extension [A <: RequestArguments](req: XtbRequest[A])
    def asText(using enc: Encoder[XtbRequest[A]]): Text = WebSocketFrame.text(req.asJson.noSpaces)
}

object XtbClient:
  final case class WsState(sessionId: Option[String]):
    def withSessionId(sessionId: String): WsState = copy(sessionId = Some(sessionId))

  def make[F[_]: Async: Logger](
      config: XtbConfig,
      backend: SttpBackend[F, Fs2Streams[F] with WebSockets]
  ): F[XtbClient[F]] =
    Monad[F].pure(LiveXtbClient(config, backend))
