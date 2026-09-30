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
import currexx.clients.broker.oanda.OandaBrokerClient.ClosePositionRequest
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, OrderExecution, OrderPlacementResult, OrderRef, TradeOrder}
import io.circe.{Codec, Json, JsonObject}
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson
import sttp.client4.WebSocketStreamBackend
import sttp.model.StatusCode

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

private[clients] trait OandaBrokerClient[F[_]] extends Fs2HttpClient[F]:
  def submit(params: BrokerParameters.Oanda, order: TradeOrder): F[OrderPlacementResult]
  def getCurrentOrders(params: BrokerParameters.Oanda, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]]
  def findEntryExecutions(params: BrokerParameters.Oanda, cp: CurrencyPair, position: TradeOrder.Position): F[List[OrderExecution]]

final private class LiveOandaBrokerClient[F[_]](
    override protected val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val config: OandaBrokerConfig
)(using
    F: Async[F],
    logger: Logger[F],
    clock: Clock[F]
) extends OandaBrokerClient[F] {
  override protected val name: String = "oanda"

  override def submit(params: BrokerParameters.Oanda, order: TradeOrder): F[OrderPlacementResult] = order match
    case enter: TradeOrder.Enter =>
      for
        accountId <- getAccountId(params)
        result    <- openPosition(accountId, params, enter)
      yield result
    case exit: TradeOrder.Exit =>
      for
        accountId <- getAccountId(params)
        position  <- getPosition(accountId, params, exit.currencyPair)
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
      current   <- getPosition(accountId, params, cp)
      tradeIds = current.flatMap(_.side(position).tradeIDs).getOrElse(Nil)
      fills <- tradeIds.traverse(getFillTransaction(accountId, params, _))
      openedAll = fills.map(_.tradeOpened.map(_.tradeID)) == tradeIds.map(Some(_))
      _ <- F.unlessA(openedAll)(logger.warn(s"$name-client/find-entry-executions: fills of trades $tradeIds did not open them"))
    yield if openedAll then fills.map(_.toExecution) else Nil

  private def getPositions(accountId: String, params: BrokerParameters.Oanda): F[List[OandaBrokerClient.Position]] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/positions")
        .auth
        .bearer(params.apiKey)
        .response(asJson[OandaBrokerClient.PositionsResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) => F.pure(res.positions)
        case Left(err)  => handleError("get-positions", err)
    }

  private def getPosition(
      accountId: String,
      params: BrokerParameters.Oanda,
      currencyPair: CurrencyPair
  ): F[Option[OandaBrokerClient.Position]] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/positions/${currencyPair.toInstrument}")
        .auth
        .bearer(params.apiKey)
        .response(asJson[OandaBrokerClient.PositionResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) =>
          F.pure(Some(res.position))
        case Left(_) if r.code == StatusCode.NotFound =>
          logger.warn(s"$name-client/get-position-404: No position for $accountId / ${currencyPair.toInstrument}") >>
            F.pure(None)
        case Left(err) =>
          handleError("get-position", err)
    }

  private def closePosition(
      accountId: String,
      params: BrokerParameters.Oanda,
      position: OandaBrokerClient.Position
  ): F[OrderPlacementResult] = {
    val request = position.toClosePositionRequest
    dispatch {
      basicRequest
        .put(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/positions/${position.instrument}/close")
        .auth
        .bearer(params.apiKey)
        .body(asJson(request))
        .response(asJson[OandaBrokerClient.ClosePositionResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) => closeOutcome(position.instrument, request, res)
        case Left(err)  => handleError("close-position", err)
    }
  }

  private def closeOutcome(
      instrument: String,
      request: ClosePositionRequest,
      res: OandaBrokerClient.ClosePositionResponse
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
    fills.traverse(Json.fromJsonObject(_).as[OandaBrokerClient.OrderFillTransaction]) match
      case Right(transactions) => F.pure(OrderPlacementResult.Filled(transactions.map(_.toExecution)))
      case Left(error)         =>
        val body = fills.map(Json.fromJsonObject(_).noSpaces).toList.mkString("\n")
        logger.error(s"$name-client/json-parsing: filled with an unparseable order fill: ${error.getMessage}\n$body") >>
          F.pure(OrderPlacementResult.FilledWithoutExecution(fills.toList.flatMap(_("orderID").flatMap(_.asString)).headOption))

  private def openPosition(accountId: String, params: BrokerParameters.Oanda, position: TradeOrder.Enter): F[OrderPlacementResult] =
    F.delay(UUID.randomUUID().toString).flatMap { clientOrderId =>
      def lookUpOrder(reason: String): F[OrderPlacementResult] =
        logger.warn(s"$name-client/open-position-$reason: looking up order $clientOrderId") >>
          findOrderOutcome(accountId, params, clientOrderId)

      dispatch {
        basicRequest
          .post(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/orders")
          .auth
          .bearer(params.apiKey)
          .body(asJson(OandaBrokerClient.OpenPositionRequest.from(position, clientOrderId)))
          .response(asJson[OandaBrokerClient.OpenPositionResponse])
      }.attempt.flatMap {
        case Left(error) => lookUpOrder(error.getClass.getSimpleName.toLowerCase)
        case Right(r)    =>
          (r.code, r.body) match
            case (StatusCode.Created, Right(res)) => F.pure(res.toResult(clientOrderId))
            case (StatusCode.Created, Left(err))  => lookUpOrder(s"unparseable: $err")
            case (StatusCode.Forbidden, _)        =>
              logger.warn(s"$name-client/open-position: Rate limited, retrying in 30s") >>
                clock.sleep(30.seconds) >> openPosition(accountId, params, position)
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

  private def getOrder(accountId: String, params: BrokerParameters.Oanda, clientOrderId: String): F[Option[OandaBrokerClient.Order]] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/orders/${s"@$clientOrderId"}")
        .auth
        .bearer(params.apiKey)
        .response(asJson[OandaBrokerClient.OrderResponse])
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
      order: OandaBrokerClient.Order
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
  ): F[OandaBrokerClient.OrderFillTransaction] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/transactions/$transactionId")
        .auth
        .bearer(params.apiKey)
        .response(asJson[OandaBrokerClient.FillTransactionResponse])
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
        .response(asJson[OandaBrokerClient.AccountsResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) if res.accounts.exists(_.id == params.accountId) => F.pure(params.accountId)
        case Right(_)  => F.raiseError(AppError.ClientFailure(name, s"Account id ${params.accountId} does not exist"))
        case Left(err) => handleError("get-account", err)
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
  private val LotSize = 100000

  final case class ClosePositionRequest(
      longUnits: String,
      shortUnits: String
  ) derives Codec.AsObject

  final case class OpenPositionRequest(order: OpenPositionOrder) derives Codec.AsObject

  // Close outcomes depend on fill presence and cancellation reasons, not transaction metadata.
  final case class ClosePositionResponse(
      lastTransactionID: String,
      longOrderFillTransaction: Option[JsonObject],
      longOrderCancelTransaction: Option[CloseOrderCancelTransaction],
      shortOrderFillTransaction: Option[JsonObject],
      shortOrderCancelTransaction: Option[CloseOrderCancelTransaction]
  ) derives Codec.AsObject:
    def fills: List[JsonObject]                                   = List(longOrderFillTransaction, shortOrderFillTransaction).flatten
    def cancellations: List[CloseOrderCancelTransaction]          = List(longOrderCancelTransaction, shortOrderCancelTransaction).flatten
    def missingFills(request: ClosePositionRequest): List[String] = List(
      Option.when(request.longUnits == "ALL" && longOrderFillTransaction.isEmpty)("long"),
      Option.when(request.shortUnits == "ALL" && shortOrderFillTransaction.isEmpty)("short")
    ).flatten

  final case class CloseOrderCancelTransaction(reason: String) derives Codec.AsObject

  final case class OpenPositionResponse(
      orderCreateTransaction: OrderTransaction,
      orderFillTransaction: Option[OrderFillTransaction],
      orderCancelTransaction: Option[OrderCancelTransaction],
      relatedTransactionIDs: List[String],
      lastTransactionID: String
  ) derives Codec.AsObject:
    def toResult(clientOrderId: String): OrderPlacementResult =
      (orderCancelTransaction, orderFillTransaction) match
        case (Some(cancel), _)  => OrderPlacementResult.Cancelled(cancel.reason)
        case (None, Some(fill)) => OrderPlacementResult.filled(fill.toExecution)
        case (None, None)       => OrderPlacementResult.Pending(OrderRef(clientOrderId, Some(orderCreateTransaction.id)))

  final case class OrderTransaction(
      id: String,
      time: Instant,
      userID: Int,
      accountID: String,
      batchID: String,
      requestID: Option[String]
  ) derives Codec.AsObject

  // Only the fields that make up an execution are decoded, so unrelated metadata cannot break fill detection.
  final case class OrderFillTransaction(
      id: String,
      orderID: String,
      time: Instant,
      units: BigDecimal,
      price: BigDecimal,
      fullVWAP: Option[BigDecimal],
      tradeOpened: Option[TradeRef],
      tradesClosed: Option[List[TradeRef]],
      tradeReduced: Option[TradeRef]
  ) derives Codec.AsObject:
    def toExecution: OrderExecution =
      OrderExecution(
        price = fullVWAP.getOrElse(price),
        time = time,
        volume = units.abs / LotSize,
        orderId = orderID,
        transactionId = id,
        tradeIds = tradeOpened.toList.map(_.tradeID) ++ tradeReduced.map(_.tradeID) ++ tradesClosed.toList.flatten.map(_.tradeID)
      )

  final case class TradeRef(tradeID: String) derives Codec.AsObject

  final case class FillTransactionResponse(transaction: OrderFillTransaction) derives Codec.AsObject

  final case class OrderCancelTransaction(
      id: String,
      time: Instant,
      userID: Int,
      accountID: String,
      batchID: String,
      requestID: Option[String],
      `type`: String,
      reason: String
  ) derives Codec.AsObject

  object OpenPositionRequest:
    def from(order: TradeOrder.Enter, clientOrderId: String): OpenPositionRequest =
      val units = order.position match
        case TradeOrder.Position.Buy  => (order.volume * LotSize).toInt
        case TradeOrder.Position.Sell => -(order.volume * LotSize).toInt
      OpenPositionRequest(
        OpenPositionOrder(
          instrument = s"${order.currencyPair.base}_${order.currencyPair.quote}",
          units = units,
          `type` = "MARKET",
          positionFill = "DEFAULT",
          clientExtensions = ClientExtensions(clientOrderId)
        )
      )

  final case class OpenPositionOrder(
      instrument: String,
      units: Int,
      `type`: String,
      positionFill: String,
      clientExtensions: ClientExtensions
  ) derives Codec.AsObject

  final case class ClientExtensions(id: String) derives Codec.AsObject

  final case class OrderResponse(order: Order) derives Codec.AsObject

  final case class Order(id: String, state: String, fillingTransactionID: Option[String]) derives Codec.AsObject

  final case class AccountsResponse(accounts: List[Account]) derives Codec.AsObject

  final case class Account(id: String) derives Codec.AsObject

  final case class PositionsResponse(positions: List[Position]) derives Codec.AsObject

  final case class PositionResponse(position: Position) derives Codec.AsObject

  final case class Position(
      instrument: String,
      long: PositionSide,
      short: PositionSide
  ) derives Codec.AsObject {
    def isOpen: Boolean                                   = long.units != 0 || short.units != 0
    def isHedged: Boolean                                 = long.units != 0 && short.units != 0
    def side(position: TradeOrder.Position): PositionSide = position match
      case TradeOrder.Position.Buy  => long
      case TradeOrder.Position.Sell => short
    def toClosePositionRequest: ClosePositionRequest =
      ClosePositionRequest(
        longUnits = if (long.units == 0) "NONE" else "ALL",
        shortUnits = if (short.units == 0) "NONE" else "ALL"
      )
    def toOpenedTradeOrder: Option[OpenedTradeOrder] =
      Option.when(isOpen) {
        val isBuy = long.units > 0
        val side  = if isBuy then long else short
        OpenedTradeOrder(
          currencyPair = CurrencyPair.fromUnsafe(instrument.replace("_", "")),
          position = if isBuy then TradeOrder.Position.Buy else TradeOrder.Position.Sell,
          openPrice = side.averagePrice.getOrElse(BigDecimal(0)),
          currentPrice = side.averagePrice
            .getOrElse(BigDecimal(0)) + Option.when(side.units != 0)(side.unrealizedPL / side.units).getOrElse(BigDecimal(0)),
          volume = side.units.abs / LotSize,
          profit = side.unrealizedPL
        )
      }
  }

  final case class PositionSide(
      units: BigDecimal,
      tradeIDs: Option[List[String]],
      averagePrice: Option[BigDecimal],
      unrealizedPL: BigDecimal
  ) derives Codec.AsObject

  def make[F[_]: {Async, Logger, Clock}](
      config: OandaBrokerConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]]
  ): F[OandaBrokerClient[F]] =
    LiveOandaBrokerClient(backend, config).pure[F]
}
