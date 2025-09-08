package currexx.clients.broker.oanda

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.flatMap.*
import currexx.clients.Fs2HttpClient
import currexx.clients.broker.BrokerParameters
import currexx.clients.broker.oanda.OandaClient.ClosePositionRequest
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, TradeOrder}
import io.circe.Codec
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson
import sttp.client4.WebSocketStreamBackend
import sttp.model.StatusCode

private[clients] trait OandaClient[F[_]] extends Fs2HttpClient[F]:
  def submit(params: BrokerParameters.Oanda, order: TradeOrder): F[Unit]
  def getCurrentOrders(params: BrokerParameters.Oanda, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]]

final private class LiveXtbClient[F[_]](
    override protected val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val config: OandaConfig
)(using
    F: Async[F],
    logger: Logger[F]
) extends OandaClient[F] {
  override protected val name: String = "oanda"

  override def submit(params: BrokerParameters.Oanda, order: TradeOrder): F[Unit] = ???

  override def getCurrentOrders(params: BrokerParameters.Oanda, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] = ???

  private def getPosition(accountId: String, params: BrokerParameters.Oanda, currencyPair: CurrencyPair): F[OandaClient.Position] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/positions/${currencyPair.toInstrument}")
        .auth
        .bearer(params.apiKey)
        .response(asJson[OandaClient.PositionResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) => F.pure(res.position)
        case Left(err)  => handleError("get-position", err)
    }

  private def closePosition(accountId: String, params: BrokerParameters.Oanda, position: OandaClient.Position): F[Unit] =
    dispatch {
      basicRequest
        .put(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/positions/${position.instrument}/close")
        .auth
        .bearer(params.apiKey)
        .body(asJson(position.toClosePositionRequest))
        .response(asStringAlways)
    }.flatMap { r =>
      r.code match {
        case StatusCode.Ok => F.unit
        case status        =>
          logger.error(s"$name-client/close-position-${status.code}\n${r.body}") >>
            F.raiseError(AppError.ClientFailure(name, s"Close position returned ${status.code}"))
      }
    }

  private def openPosition(accountId: String, params: BrokerParameters.Oanda, position: TradeOrder.Enter): F[Unit] =
    dispatch {
      basicRequest
        .post(uri"${config.baseUri(params.demo)}/v3/accounts/$accountId/orders")
        .auth
        .bearer(params.apiKey)
        .body(asJson(OandaClient.OpenPositionRequest.from(position)))
        .response(asStringAlways)
    }.flatMap { r =>
      r.code match {
        case StatusCode.Created => F.unit
        case status             =>
          logger.error(s"$name-client/open-position-${status.code}\n${r.body}") >>
            F.raiseError(AppError.ClientFailure(name, s"Open position returned ${status.code}"))
      }
    }

  private def getAccountId(params: BrokerParameters.Oanda): F[String] =
    dispatch {
      basicRequest
        .get(uri"${config.baseUri(params.demo)}/v3/accounts")
        .auth
        .bearer(params.apiKey)
        .response(asJson[OandaClient.AccountsResponse])
    }.flatMap { r =>
      r.body match
        case Right(res) if res.accounts.nonEmpty => F.pure(res.accounts.head.id)
        case Right(_)                            => F.raiseError(AppError.ClientFailure(name, s"$name returned empty accounts list"))
        case Left(err)                           => handleError("get-account", err)
    }

  private def handleError[A](endpoint: String, error: ResponseException[String]): F[A] =
    error match
      case ResponseException.DeserializationException(responseBody, error, _) =>
        logger.error(s"$name-client/json-parsing: ${error.getMessage}\n$responseBody") >>
          F.raiseError(AppError.JsonParsingFailure(responseBody, s"${name} client returned $error"))
      case ResponseException.UnexpectedStatusCode(body, meta) =>
        logger.error(s"$name-client/${meta.code.code}\n$body") >>
          F.raiseError(AppError.ClientFailure(name, s"$endpoint returned ${meta.code}"))

  extension (cp: CurrencyPair) private def toInstrument: String = s"${cp.base}_${cp.quote}"

  extension (c: OandaConfig)
    private def baseUri(demo: Boolean): String =
      if (demo) c.demoBaseUri else c.liveBaseUri
}

object OandaClient {
  private val LotSize = 100000

  final case class ClosePositionRequest(
      longUnits: String,
      shortUnits: String
  ) derives Codec.AsObject

  final case class OpenPositionRequest(order: OpenPositionOrder) derives Codec.AsObject

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

  final case class PositionResponse(position: Position) derives Codec.AsObject

  final case class Position(
      instrument: String,
      long: PositionSide,
      short: PositionSide
  ) derives Codec.AsObject {
    def toClosePositionRequest: ClosePositionRequest =
      ClosePositionRequest(
        longUnits = if (long.units == "0") "NONE" else "ALL",
        shortUnits = if (short.units == "0") "NONE" else "ALL"
      )
    def toOpenedTradeOrder: Option[OpenedTradeOrder] =
      Option.when(long.units != "0" || short.units != "0") {
        val (position, volume, profit, openPrice) =
          if (long.units != "0")
            (
              TradeOrder.Position.Buy,
              BigDecimal(long.units) / LotSize,
              BigDecimal(long.trueUnrealizedPL),
              BigDecimal(long.averagePrice.getOrElse("0"))
            )
          else
            (
              TradeOrder.Position.Sell,
              BigDecimal(short.units) / LotSize,
              BigDecimal(short.trueUnrealizedPL),
              BigDecimal(short.averagePrice.getOrElse("0"))
            )
        OpenedTradeOrder(
          currencyPair = CurrencyPair.fromUnsafe(instrument.replace("_", "")),
          position = position,
          openPrice = openPrice,
          volume = volume,
          profit = profit
        )
      }
  }

  final case class PositionSide(
      units: String,
      tradeIDs: Option[List[String]],
      averagePrice: Option[String],
      trueUnrealizedPL: String
  ) derives Codec.AsObject
}
