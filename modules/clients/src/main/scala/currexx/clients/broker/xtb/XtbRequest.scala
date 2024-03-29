package currexx.clients.broker.xtb

import cats.syntax.option.*
import currexx.domain.market.{CurrencyPair, TradeOrder}
import io.circe.Codec

sealed trait RequestArguments
object RequestArguments:
  final case class Login(
      userId: String,
      password: String
  ) extends RequestArguments
      derives Codec.AsObject

  final case class TradeTransInfo(
      cmd: Option[Int],
      `type`: Int,
      symbol: String,
      customComment: String,
      price: BigDecimal,
      volume: BigDecimal,
      sl: Option[BigDecimal] = None,
      tp: Option[BigDecimal] = None,
      offset: Option[BigDecimal] = None,
      order: Option[Long] = None,
      expiration: Option[Long] = None
  ) derives Codec.AsObject

  final case class Transaction(tradeTransInfo: TradeTransInfo) extends RequestArguments derives Codec.AsObject

  final case class SymbolInfo(symbol: String) extends RequestArguments derives Codec.AsObject

  final case class Trades(openedOnly: Boolean) extends RequestArguments derives Codec.AsObject

final case class XtbRequest[A <: RequestArguments](
    command: String,
    streamSessionId: Option[String],
    arguments: Option[A]
) derives Codec.AsObject

object XtbRequest {
  def login(userId: String, password: String): XtbRequest[RequestArguments.Login] =
    XtbRequest("login", None, RequestArguments.Login(userId, password).some)

  def currentTrades(sessionId: String): XtbRequest[RequestArguments.Trades] =
    XtbRequest(
      "getTrades",
      Some(sessionId),
      RequestArguments.Trades(true).some
    )

  def openTransaction(
      sessionId: String,
      order: TradeOrder.Enter
  ): XtbRequest[RequestArguments.Transaction] =
    XtbRequest(
      "tradeTransaction",
      Some(sessionId),
      RequestArguments
        .Transaction(
          RequestArguments.TradeTransInfo(
            `type` = 0,
            cmd = Some(if (order.position == TradeOrder.Position.Buy) 0 else 1),
            symbol = order.currencyPair.toString,
            customComment = s"Currexx - ${TradeOrder.Position.Buy.toString} ${order.currencyPair}",
            offset = None,
            volume = order.volume,
            price = order.price,
            sl = None,
            tp = None
          )
        )
        .some
    )

  def closeTransaction(
      sessionId: String,
      cp: CurrencyPair,
      data: XtbResponse.TradeData
  ): XtbRequest[RequestArguments.Transaction] =
    XtbRequest(
      "tradeTransaction",
      Some(sessionId),
      RequestArguments
        .Transaction(
          RequestArguments.TradeTransInfo(
            `type` = 2,
            cmd = None,
            symbol = cp.toString,
            customComment = s"Currexx - Close $cp",
            price = data.close_price,
            volume = data.volume,
            order = Some(data.position)
          )
        )
        .some
    )

  def symbolInfo(sessionId: String, cp: CurrencyPair): XtbRequest[RequestArguments.SymbolInfo] =
    XtbRequest(
      "getSymbol",
      Some(sessionId),
      Some(RequestArguments.SymbolInfo(cp.toString))
    )

  def allSymbolsInfo(sessionId: String): XtbRequest[RequestArguments.SymbolInfo] =
    XtbRequest("getAllSymbols", Some(sessionId), None)
}
