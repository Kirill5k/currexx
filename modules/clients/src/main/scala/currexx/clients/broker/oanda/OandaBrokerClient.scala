package currexx.clients.broker.oanda

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.Fs2HttpClient
import currexx.clients.broker.BrokerParameters
import currexx.clients.broker.oanda.OandaBrokerClient.ClosePositionRequest
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, TradeOrder}
import io.circe.Codec
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson
import sttp.client4.WebSocketStreamBackend
import sttp.model.StatusCode

import java.time.Instant
import scala.concurrent.duration.*

private[clients] trait OandaBrokerClient[F[_]] extends Fs2HttpClient[F]:
  def submit(params: BrokerParameters.Oanda, order: TradeOrder): F[Unit]
  def getCurrentOrders(params: BrokerParameters.Oanda, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]]

final private class LiveOandaBrokerClient[F[_]](
    override protected val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val config: OandaBrokerConfig
)(using
    F: Async[F],
    logger: Logger[F],
    clock: Clock[F]
) extends OandaBrokerClient[F] {
  override protected val name: String = "oanda"

  override def submit(params: BrokerParameters.Oanda, order: TradeOrder): F[Unit] = order match
    case enter: TradeOrder.Enter =>
      for
        accountId <- getAccountId(params)
        _         <- openPosition(accountId, params, enter)
      yield ()
    case exit: TradeOrder.Exit =>
      for
        accountId <- getAccountId(params)
        position  <- getPosition(accountId, params, exit.currencyPair)
        _         <- F.ifM(F.pure(position.exists(_.isOpen)))(
          closePosition(accountId, params, position.get),
          logger.warn(s"$name-client: No open position for $accountId / ${exit.currencyPair}")
        )
      yield ()

  override def getCurrentOrders(params: BrokerParameters.Oanda, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] =
    for
      accountId <- getAccountId(params)
      positions <- getPositions(accountId, params)
      instruments  = cps.toList.map(_.toInstrument).toSet
      openedOrders = positions.filter(p => instruments.contains(p.instrument)).flatMap(_.toOpenedTradeOrder)
    yield openedOrders

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
          logger.warn(s"$name-client/get-position-404: No position for $accountId / ${currencyPair.toInstrument}").as(None)
        case Left(err) =>
          handleError("get-position", err)
    }

  private def closePosition(accountId: String, params: BrokerParameters.Oanda, position: OandaBrokerClient.Position): F[Unit] =
    dispatch {
      basicRequest
        .put(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/positions/${position.instrument}/close")
        .auth
        .bearer(params.apiKey)
        .body(asJson(position.toClosePositionRequest))
        .response(asJson[OandaBrokerClient.ClosePositionResponse])
    }.flatMap { r =>
      r.body match
        case Right(_)  => F.unit
        case Left(err) => handleError("close-position", err)
    }

  private def openPosition(accountId: String, params: BrokerParameters.Oanda, position: TradeOrder.Enter): F[Unit] =
    dispatch {
      basicRequest
        .post(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/orders")
        .auth
        .bearer(params.apiKey)
        .body(asJson(OandaBrokerClient.OpenPositionRequest.from(position)))
        .response(asJson[OandaBrokerClient.OpenPositionResponse])
    }.flatMap { r =>
      r.code match
        case StatusCode.Created =>
          r.body match
            case Right(res) =>
              val orderId  = res.orderCreateTransaction.id
              val fillInfo = res.orderFillTransaction
                .map { fill =>
                  val priceInfo = fill.tradeOpened.map(t => s"@ ${t.price}").orElse(
                    fill.tradesClosed.flatMap(_.headOption).map(t => s"@ ${t.price}")
                  ).orElse(
                    fill.tradeReduced.map(t => s"@ ${t.price}")
                  ).getOrElse("")
                  s"filled ${fill.units} units $priceInfo (P/L: ${fill.pl})"
                }
                .getOrElse("pending")
              val cancelInfo = res.orderCancelTransaction.map(c => s" [CANCELLED: ${c.`type`}]").getOrElse("")
              val txIds      = res.relatedTransactionIDs.mkString(", ")

              logger.info(
                s"$name-client/open-position-success: ${position.currencyPair} ${position.position} ${position.volume} lots - " +
                  s"$fillInfo$cancelInfo (orderID: $orderId, txID: ${res.lastTransactionID}, related: [$txIds])"
              ) >> F.whenA(res.orderCancelTransaction.isDefined)(
                logger.warn(s"$name-client/open-position: Order was immediately cancelled for ${position.currencyPair}")
              )
            case Left(err) =>
              // Fallback: log raw response if parsing fails
              logger.warn(s"$name-client/open-position: Created but couldn't parse response: $err") >> F.unit
        case StatusCode.Forbidden =>
          logger.warn(s"$name-client/open-position: Rate limited, retrying in 30s") >>
            clock.sleep(30.seconds) >> openPosition(accountId, params, position)
        case status =>
          val errorBody = r.body.fold(identity, _ => "")
          logger.error(s"$name-client/open-position-${status.code}\n$errorBody") >>
            F.raiseError(AppError.ClientFailure(name, s"Open position returned ${status.code}"))
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

  private def handleError[A](endpoint: String, error: Any): F[A] =
    error match
      case ResponseException.DeserializationException(responseBody, error, _) =>
        logger.error(s"$name-client/json-parsing: ${error.getMessage}\n$responseBody") >>
          F.raiseError(AppError.JsonParsingFailure(responseBody, s"${name} client returned $error"))
      case ResponseException.UnexpectedStatusCode(body, meta) =>
        val errorMessage = body.toString.trim
        logger.error(s"$name-client/${meta.code.code}: $errorMessage") >>
          F.raiseError(AppError.ClientFailure(name, s"$endpoint returned ${meta.code}: $errorMessage"))

  extension (cp: CurrencyPair) private def toInstrument: String = s"${cp.base}_${cp.quote}"

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

  final case class ClosePositionResponse(lastTransactionID: String) derives Codec.AsObject

  final case class OpenPositionResponse(
      orderCreateTransaction: OrderTransaction,
      orderFillTransaction: Option[OrderFillTransaction],
      orderCancelTransaction: Option[OrderCancelTransaction],
      relatedTransactionIDs: List[String],
      lastTransactionID: String
  ) derives Codec.AsObject

  final case class OrderTransaction(
      id: String,
      time: Instant,
      userID: Int,
      accountID: String,
      batchID: String,
      requestID: String,
  ) derives Codec.AsObject

  final case class OrderFillTransaction(
      id: String,
      time: Instant,
      userID: Int,
      accountID: String,
      batchID: String,
      requestID: String,
      `type`: String,
      units: String,
      pl: BigDecimal,
      tradeOpened: Option[TradeOpened],
      tradesClosed: Option[List[TradeClosed]],
      tradeReduced: Option[TradeReduced]
  ) derives Codec.AsObject

  final case class TradeOpened(
      tradeID: String,
      units: String,
      price: BigDecimal
  ) derives Codec.AsObject

  final case class TradeClosed(
      tradeID: String,
      units: String,
      price: BigDecimal,
      realizedPL: BigDecimal
  ) derives Codec.AsObject

  final case class TradeReduced(
      tradeID: String,
      units: String,
      price: BigDecimal,
      realizedPL: BigDecimal
  ) derives Codec.AsObject

  final case class OrderCancelTransaction(
      id: String,
      time: Instant,
      userID: Int,
      accountID: String,
      batchID: String,
      requestID: String,
      `type`: String
  ) derives Codec.AsObject

  object OpenPositionRequest:
    def from(order: TradeOrder.Enter): OpenPositionRequest =
      val units = order.position match
        case TradeOrder.Position.Buy  => (order.volume * LotSize).toInt
        case TradeOrder.Position.Sell => -(order.volume * LotSize).toInt
      OpenPositionRequest(
        OpenPositionOrder(
          instrument = s"${order.currencyPair.base}_${order.currencyPair.quote}",
          units = units,
          `type` = "MARKET",
          positionFill = "DEFAULT"
        )
      )

  final case class OpenPositionOrder(
      instrument: String,
      units: Int,
      `type`: String,
      positionFill: String
  ) derives Codec.AsObject

  final case class AccountsResponse(accounts: List[Account]) derives Codec.AsObject

  final case class Account(id: String) derives Codec.AsObject

  final case class PositionsResponse(positions: List[Position]) derives Codec.AsObject

  final case class PositionResponse(position: Position) derives Codec.AsObject

  final case class Position(
      instrument: String,
      long: PositionSide,
      short: PositionSide
  ) derives Codec.AsObject {
    def isOpen: Boolean                              = long.units > 0 || short.units > 0
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
          profit = side.trueUnrealizedPL
        )
      }
  }

  final case class PositionSide(
      units: BigDecimal,
      tradeIDs: Option[List[String]],
      averagePrice: Option[BigDecimal],
      trueUnrealizedPL: BigDecimal,
      unrealizedPL: BigDecimal
  ) derives Codec.AsObject

  def make[F[_]: {Async, Logger, Clock}](
      config: OandaBrokerConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]]
  ): F[OandaBrokerClient[F]] =
    Monad[F].pure(LiveOandaBrokerClient(backend, config))
}
