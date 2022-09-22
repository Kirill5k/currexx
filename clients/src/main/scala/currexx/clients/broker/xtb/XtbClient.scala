package currexx.clients.broker.xtb

import cats.Monad
import cats.effect.{Async, Ref}
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.clients.HttpClient
import currexx.clients.broker.BrokerParameters
import currexx.clients.broker.xtb.XtbResponse.TradeData
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, TradeOrder}
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
import java.time.Instant

private[clients] trait XtbClient[F[_]] extends HttpClient[F]:
  def submit(params: BrokerParameters.Xtb, pair: CurrencyPair, order: TradeOrder): F[Unit]
  def getCurrentOrder(params: BrokerParameters.Xtb, cp: CurrencyPair): F[Option[OpenedTradeOrder]]

final private class LiveXtbClient[F[_]](
    private val config: XtbConfig,
    override protected val backend: SttpBackend[F, Fs2Streams[F] with WebSockets]
)(using
    F: Async[F],
    logger: Logger[F]
) extends XtbClient[F] {

  override protected val name: String                                   = "xtb"
  override protected val delayBetweenConnectionFailures: FiniteDuration = 5.seconds

  override def getCurrentOrder(params: BrokerParameters.Xtb, cp: CurrencyPair): F[Option[OpenedTradeOrder]] =
    for
      state <- initEmptyState
      _ <- basicRequest
        .response(asWebSocketStream(Fs2Streams[F])(orderRetrievalProcess(state, params, cp)))
        .get(uri"${config.baseUri}/${if (params.demo) "demo" else "real"}")
        .send(backend)
        .void
      retrievedOrder <- state.get.map(_.retrievedOrder)
    yield retrievedOrder.map { td =>
      OpenedTradeOrder(
        cp,
        if (td.cmd == 0) TradeOrder.Position.Buy else TradeOrder.Position.Sell,
        td.close_price,
        td.open_price,
        Instant.ofEpochMilli(td.open_time),
        td.volume,
        td.profit
      )
    }

  override def submit(params: BrokerParameters.Xtb, cp: CurrencyPair, order: TradeOrder): F[Unit] =
    initEmptyState.flatMap { state =>
      basicRequest
        .response(
          asWebSocketStream(Fs2Streams[F])(
            order match
              case TradeOrder.Exit         => orderClosureProcess(state, params, cp)
              case enter: TradeOrder.Enter => orderPlacementProcess(state, params, cp, enter)
          )
        )
        .get(uri"${config.baseUri}/${if (params.demo) "demo" else "real"}")
        .send(backend)
        .void
    }

  private def orderPlacementProcess(
      state: Ref[F, XtbClient.WsState],
      params: BrokerParameters.Xtb,
      cp: CurrencyPair,
      order: TradeOrder.Enter
  ): Pipe[F, WebSocketFrame.Data[_], WebSocketFrame] = { input =>
    login(params) ++
      input
        .through(parseXtbResponse)
        .flatMap {
          case XtbResponse.Login(sessionId) =>
            Stream.eval(state.update(_.withSessionId(sessionId))).drain ++
              Stream.emit(XtbRequest.symbolInfo(sessionId, cp).asText)
          case XtbResponse.SymbolInfo(price) =>
            obtainSessionId(state).map(sid => XtbRequest.openTransaction(sid, cp, order, price).asText)
          case XtbResponse.OrderPlacement(_) => Stream.emit(WebSocketFrame.close)
          case error: XtbResponse.Error      => handError(params.userId, error)
          case _                             => Stream.empty
        }
  }

  private def orderClosureProcess(
      state: Ref[F, XtbClient.WsState],
      params: BrokerParameters.Xtb,
      cp: CurrencyPair
  ): Pipe[F, WebSocketFrame.Data[_], WebSocketFrame] = { input =>
    login(params) ++
      input
        .through(parseXtbResponse)
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
          case XtbResponse.Error("SE199", _) =>
            Stream.logError(s"$name-client/server-${params.userId}: failed to close transaction for $cp") ++
              obtainSessionId(state).delayBy(delayBetweenConnectionFailures).map(sid => XtbRequest.currentTrades(sid).asText)
          case error: XtbResponse.Error => handError(params.userId, error)
          case _                        => Stream.empty
        }
  }

  private def orderRetrievalProcess(
      state: Ref[F, XtbClient.WsState],
      params: BrokerParameters.Xtb,
      cp: CurrencyPair
  ): Pipe[F, WebSocketFrame.Data[_], WebSocketFrame] = { input =>
    login(params) ++
      input
        .through(parseXtbResponse)
        .flatMap {
          case XtbResponse.Login(sessionId) =>
            Stream.eval(state.update(_.withSessionId(sessionId))).drain ++
              Stream.emit(XtbRequest.currentTrades(sessionId).asText)
          case XtbResponse.Trades(trades) =>
            Stream.eval(state.update(_.withRetrievedOrder(trades.find(_.symbol == cp.toString)))).drain ++
              Stream.emit(WebSocketFrame.close)
          case error: XtbResponse.Error => handError(params.userId, error)
          case _                        => Stream.empty
        }
  }

  private val parseXtbResponse: Pipe[F, WebSocketFrame.Data[_], XtbResponse] = _.map {
    case WebSocketFrame.Text(jsonPayload, _, _) => XtbResponse.fromJson(jsonPayload)
    case WebSocketFrame.Binary(bytes, _, _)     => XtbResponse.fromJson(new String(bytes, StandardCharsets.UTF_8))
    case _ | null                               => Right(XtbResponse.Void)
  }.rethrow

  private def login(params: BrokerParameters.Xtb): Stream[F, Text] =
    Stream.emit(XtbRequest.login(params.userId, params.password).asText)

  private def initEmptyState: F[Ref[F, XtbClient.WsState]] =
    Ref.of(XtbClient.WsState(None))

  private def obtainSessionId(state: Ref[F, XtbClient.WsState]): Stream[F, String] =
    Stream.eval(state.get.map(_.sessionId.toRight(AppError.ClientFailure(name, "no session id")))).rethrow

  private def handError(userId: String, error: XtbResponse.Error): Stream[F, WebSocketFrame] =
    error match
      case XtbResponse.Error("BE005", desc) =>
        Stream.emit(WebSocketFrame.close) ++
          Stream.logError(s"$name-client/forbidden-$userId: $desc") ++
          Stream.raiseError(AppError.AccessDenied(s"Failed to authenticate with $name: $desc"))
      case XtbResponse.Error(code, desc) =>
        Stream.emit(WebSocketFrame.close) ++
          Stream.logError(s"$name-client/error-$userId: $desc") ++
          Stream.raiseError(AppError.ClientFailure(name, s"$code - $desc"))

  extension [A <: RequestArguments](req: XtbRequest[A])
    def asText(using enc: Encoder[XtbRequest[A]]): Text = WebSocketFrame.text(req.asJson.noSpaces)
}

object XtbClient:
  final case class WsState(sessionId: Option[String], retrievedOrder: Option[TradeData] = None):
    def withRetrievedOrder(retrievedOrder: Option[TradeData]): WsState = copy(retrievedOrder = retrievedOrder)
    def withSessionId(sessionId: String): WsState                      = copy(sessionId = Some(sessionId))

  def make[F[_]: Async: Logger](
      config: XtbConfig,
      backend: SttpBackend[F, Fs2Streams[F] with WebSockets]
  ): F[XtbClient[F]] =
    Monad[F].pure(LiveXtbClient(config, backend))
