package currexx.clients.broker.oanda

import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.clients.Fs2HttpClient
import currexx.clients.broker.BrokerParameters
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, OrderExecution, OrderPlacementResult, OrderRef, TradeOrder}
import io.circe.{Json, JsonObject}
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson
import sttp.client4.WebSocketStreamBackend
import sttp.model.StatusCode

import java.util.UUID
import scala.concurrent.duration.*

private[clients] trait OandaBrokerClient[F[_]] extends Fs2HttpClient[F]:
  def submit(params: BrokerParameters.Oanda, order: TradeOrder): F[OrderPlacementResult]
  def getCurrentOrders(params: BrokerParameters.Oanda, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]]
  def findEntryExecutions(params: BrokerParameters.Oanda, cp: CurrencyPair, position: TradeOrder.Position): F[List[OrderExecution]]

final private class LiveOandaBrokerClient[F[_]](
    override protected val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val config: OandaBrokerConfig,
    override protected val maxRetries: Int,
    private val maxMarketHaltRetries: Int
)(using
    F: Async[F],
    logger: Logger[F],
    clock: Clock[F]
) extends OandaBrokerClient[F] {
  override protected val name: String                                    = "oanda"
  override protected val additionalRetryableStatusCodes: Set[StatusCode] = Set(StatusCode.Unauthorized)

  override def submit(params: BrokerParameters.Oanda, order: TradeOrder): F[OrderPlacementResult] = order match
    case enter: TradeOrder.Enter =>
      for
        accountId <- getAccountId(params)
        result    <- openPosition(accountId, params, enter)
      yield result
    case exit: TradeOrder.Exit =>
      for
        accountId <- getAccountId(params)
        position  <- getPosition(accountId, params, exit.currencyPair.toInstrument)
        result    <- F.ifM(F.pure(position.exists(_.isOpen)))(
          closePosition(accountId, params, position.get),
          F.pure(OrderPlacementResult.NoPosition)
        )
      yield result

  override def getCurrentOrders(params: BrokerParameters.Oanda, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] =
    for
      accountId <- getAccountId(params)
      positions <- getPositions(accountId, params)
      instruments = cps.toList.map(_.toInstrument).toSet
      requested   = positions.filter(p => instruments.contains(p.instrument))
      hedged      = requested.filter(_.isHedged).map(_.instrument)
      // An opened order describes a single side, so reporting either side of a hedged position would misstate it
      _ <- F.whenA(hedged.nonEmpty)(
        clientFailure[Unit](s"get-positions for ${hedged.mkString(", ")} has both sides open, hedging is not supported")
      )
    yield requested.flatMap(_.toOpenedTradeOrder)

  // Oanda derives a trade's ID from the ID of the fill transaction that opened it.
  // Only a complete set of fills is returned, since a partial one would misstate the position's entry price.
  override def findEntryExecutions(
      params: BrokerParameters.Oanda,
      cp: CurrencyPair,
      position: TradeOrder.Position
  ): F[List[OrderExecution]] =
    for
      accountId <- getAccountId(params)
      current   <- getPosition(accountId, params, cp.toInstrument)
      tradeIds = current.flatMap(_.side(position).tradeIDs).getOrElse(Nil)
      fills <- tradeIds.traverse(getFillTransaction(accountId, params, _))
      openedAll = fills.map(_.tradeOpened.map(_.tradeID)) == tradeIds.map(Some(_))
      _ <- F.unlessA(openedAll)(logger.warn(s"$name-client/find-entry-executions: fills of trades $tradeIds did not open them"))
    yield if openedAll then fills.map(_.toExecution) else Nil

  private def getPositions(accountId: String, params: BrokerParameters.Oanda): F[List[Position]] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/positions")
        .auth
        .bearer(params.apiKey)
        .response(asJson[PositionsResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) => F.pure(res.positions)
        case Left(err)  => handleError("get-positions", err)
    }

  private def getPosition(
      accountId: String,
      params: BrokerParameters.Oanda,
      instrument: String
  ): F[Option[Position]] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/positions/$instrument")
        .auth
        .bearer(params.apiKey)
        .response(asJson[PositionResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) =>
          F.pure(Some(res.position))
        case Left(_) if r.code == StatusCode.NotFound =>
          logger.warn(s"$name-client/get-position-404: No position for $accountId / $instrument") >>
            F.pure(None)
        case Left(err) =>
          handleError("get-position", err)
    }

  private def closePosition(
      accountId: String,
      params: BrokerParameters.Oanda,
      position: Position,
      marketHaltAttempt: Int = 0
  ): F[OrderPlacementResult] = {
    val request = position.toClosePositionRequest
    dispatch {
      basicRequest
        .put(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/positions/${position.instrument}/close")
        .auth
        .bearer(params.apiKey)
        .body(asJson(request))
        .response(asJson[ClosePositionResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) if res.isMarketHalted && marketHaltAttempt < maxMarketHaltRetries =>
          retryAfterMarketHalt(position.instrument, marketHaltAttempt) {
            getPosition(accountId, params, position.instrument).flatMap {
              case current if current.forall(p => !p.isOpen) =>
                F.pure(OrderPlacementResult.NoPosition)
              case Some(current) if position.hasSameTradesAs(current) =>
                closePosition(accountId, params, current, marketHaltAttempt + 1)
              case _ =>
                logger.warn(s"$name-client/close-position: ${position.instrument} position changed while halted, not retrying the close") >>
                  closeOutcome(position.instrument, request, res)
            }
          }
        case Right(res) => closeOutcome(position.instrument, request, res)
        case Left(err)  => handleError("close-position", err)
    }
  }

  private def closeOutcome(
      instrument: String,
      request: ClosePositionRequest,
      res: ClosePositionResponse
  ): F[OrderPlacementResult] =
    val missingFills = res.missingFills(request)
    res.fills match
      case _ if res.cancellations.nonEmpty =>
        F.pure(OrderPlacementResult.Cancelled(res.cancellations.map(_.reason).mkString("; ")))
      case _ if missingFills.nonEmpty =>
        clientFailure(s"close-position for $instrument missing fill confirmation for ${missingFills.mkString(", ")}")
      case fill :: rest => decodeFills(NonEmptyList(fill, rest))
      case Nil          => clientFailure(s"close-position for $instrument returned no fills")

  private def clientFailure[A](message: String): F[A] =
    logger.error(s"$name-client/$message") >> F.raiseError(AppError.ClientFailure(name, message))

  // The fills' presence already confirms the close, so fills that cannot be decoded only lose their execution details
  private def decodeFills(fills: NonEmptyList[JsonObject]): F[OrderPlacementResult] =
    fills.traverse(Json.fromJsonObject(_).as[OrderFillTransaction]) match
      case Right(transactions) => F.pure(OrderPlacementResult.Filled(transactions.map(_.toExecution)))
      case Left(error)         =>
        val body = fills.map(Json.fromJsonObject(_).noSpaces).toList.mkString("\n")
        logger.error(s"$name-client/json-parsing: filled with an unparseable order fill: ${error.getMessage}\n$body") >>
          F.pure(OrderPlacementResult.FilledWithoutExecution(fills.toList.flatMap(_("orderID").flatMap(_.asString)).headOption))

  private def retryAfterMarketHalt[A](instrument: String, attempt: Int)(retry: => F[A]): F[A] =
    logger.warn(s"$name-client/$instrument: MARKET_HALTED, retrying in 3 minutes (retry ${attempt + 1}/$maxMarketHaltRetries)") >>
      clock.sleep(3.minutes) >> retry

  private def openPosition(
      accountId: String,
      params: BrokerParameters.Oanda,
      position: TradeOrder.Enter,
      marketHaltAttempt: Int = 0
  ): F[OrderPlacementResult] =
    F.delay(UUID.randomUUID().toString).flatMap { clientOrderId =>
      def lookUpOrder(reason: String): F[OrderPlacementResult] =
        logger.warn(s"$name-client/open-position-$reason: looking up order $clientOrderId") >>
          findOrderOutcome(accountId, params, clientOrderId)

      dispatch {
        basicRequest
          .post(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/orders")
          .auth
          .bearer(params.apiKey)
          .body(asJson(OpenPositionRequest.from(position, clientOrderId)))
          .response(asJson[OpenPositionResponse])
      }.attempt.flatMap {
        case Left(error) => lookUpOrder(error.getClass.getSimpleName.toLowerCase)
        case Right(r)    =>
          (r.code, r.body) match
            case (StatusCode.Created, Right(res)) if res.isMarketHalted && marketHaltAttempt < maxMarketHaltRetries =>
              retryAfterMarketHalt(position.currencyPair.toInstrument, marketHaltAttempt) {
                openPosition(accountId, params, position, marketHaltAttempt + 1)
              }
            case (StatusCode.Created, Right(res)) => F.pure(res.toResult(clientOrderId))
            case (StatusCode.Created, Left(err))  => lookUpOrder(s"unparseable: $err")
            case (StatusCode.Forbidden, _)        =>
              logger.warn(s"$name-client/open-position: Rate limited, retrying in 30s") >>
                clock.sleep(30.seconds) >> openPosition(accountId, params, position, marketHaltAttempt)
            case (status, _) if status.isServerError => lookUpOrder(status.code.toString)
            case (status, body)                      =>
              logger.error(s"$name-client/open-position-${status.code}\n${body.fold(identity, _ => "")}") >>
                F.raiseError(AppError.ClientFailure(name, s"Open position returned ${status.code}"))
      }
    }

  // Resubmitting after a lost response could open a duplicate position, so the order is looked up by its client id instead
  private def findOrderOutcome(accountId: String, params: BrokerParameters.Oanda, clientOrderId: String): F[OrderPlacementResult] =
    clock.sleep(5.seconds) >> getOrder(accountId, params, clientOrderId).flatMap {
      case None =>
        F.raiseError(AppError.ClientFailure(name, s"Open position order $clientOrderId was not created"))
      case Some(order) if order.state == "FILLED"    => getOrderExecution(accountId, params, order)
      case Some(order) if order.state == "CANCELLED" => F.pure(OrderPlacementResult.Cancelled(s"Order ${order.id} was cancelled"))
      case Some(order)                               => F.pure(OrderPlacementResult.Pending(OrderRef(clientOrderId, Some(order.id))))
    }

  private def getOrder(accountId: String, params: BrokerParameters.Oanda, clientOrderId: String): F[Option[Order]] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/orders/${s"@$clientOrderId"}")
        .auth
        .bearer(params.apiKey)
        .response(asJson[OrderResponse])
    }.flatMap { r =>
      r.body match
        case Right(res)                               => F.pure(Some(res.order))
        case Left(_) if r.code == StatusCode.NotFound => F.pure(None)
        case Left(err)                                => handleError("get-order", err)
    }

  // The order is confirmed as filled, so failing to fetch its execution details must not discard the fill
  private def getOrderExecution(
      accountId: String,
      params: BrokerParameters.Oanda,
      order: Order
  ): F[OrderPlacementResult] =
    val withoutExecution = OrderPlacementResult.FilledWithoutExecution(Some(order.id))
    order.fillingTransactionID match
      case None =>
        logger.error(s"$name-client/get-order: Order ${order.id} is filled but has no filling transaction").as(withoutExecution)
      case Some(transactionId) =>
        getFillTransaction(accountId, params, transactionId)
          .map(fill => OrderPlacementResult.filled(fill.toExecution))
          .handleErrorWith { error =>
            logger
              .error(s"$name-client/get-transaction: Order ${order.id} is filled but its fill $transactionId is unavailable: $error")
              .as(withoutExecution)
          }

  private def getFillTransaction(
      accountId: String,
      params: BrokerParameters.Oanda,
      transactionId: String
  ): F[OrderFillTransaction] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/transactions/$transactionId")
        .auth
        .bearer(params.apiKey)
        .response(asJson[FillTransactionResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) => F.pure(res.transaction)
        case Left(err)  => handleError("get-transaction", err)
    }

  private def getAccountId(params: BrokerParameters.Oanda): F[String] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts")
        .auth
        .bearer(params.apiKey)
        .response(asJson[AccountsResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) if res.accounts.exists(_.id == params.accountId) => F.pure(params.accountId)
        case Right(_)  => F.raiseError(AppError.ClientFailure(name, s"Account id ${params.accountId} does not exist"))
        case Left(err) => handleError(s"get-account-${params.accountId}", err)
    }

  private def handleError[A](endpoint: String, error: ResponseException[String]): F[A] =
    error match
      case ResponseException.DeserializationException(responseBody, error, _) =>
        logger.error(s"$name-client/json-parsing: ${error.getMessage}\n$responseBody") >>
          F.raiseError(AppError.JsonParsingFailure(responseBody, s"${name} client returned $error"))
      case ResponseException.UnexpectedStatusCode(body, meta) =>
        val errorMessage = body.trim
        logger.error(s"$name-client/${meta.code.code}: $errorMessage") >>
          F.raiseError(AppError.ClientFailure(name, s"$endpoint returned ${meta.code}: $errorMessage"))

  extension (cp: CurrencyPair)
    private def toInstrument: String =
      s"${cp.base}_${cp.quote}"

  extension (c: OandaBrokerConfig)
    private def baseUri(demo: Boolean): String =
      if (demo) c.demoBaseUri else c.liveBaseUri
}

object OandaBrokerClient {
  val LotSize = 100000

  def make[F[_]: {Async, Logger, Clock}](
      config: OandaBrokerConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]],
      maxRetries: Int = 6,
      maxMarketHaltRetries: Int = 3
  ): F[OandaBrokerClient[F]] =
    LiveOandaBrokerClient(backend, config, maxRetries, maxMarketHaltRetries).pure[F]
}
