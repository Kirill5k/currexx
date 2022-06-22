package currexx.clients.broker.xtb

import currexx.domain.market.{CurrencyPair, TradeOrder}
import io.circe.Codec

import java.time.ZonedDateTime

sealed trait RequestArguments
object RequestArguments:
  final case class Login(
      userId: String,
      password: String
  ) extends RequestArguments
      derives Codec.AsObject
  final case class Trade(
      cmd: Option[Int],
      `type`: Int,
      symbol: String,
      price: Option[BigDecimal],
      sl: Option[BigDecimal],
      tp: Option[BigDecimal],
      offset: Option[BigDecimal],
      volume: Option[BigDecimal],
      customComment: String
  ) extends RequestArguments
      derives Codec.AsObject

final case class XtbRequest[A <: RequestArguments](
    command: String,
    streamSessionId: Option[String],
    arguments: A
) derives Codec.AsObject

object XtbRequest {
  def login(userId: String, password: String): XtbRequest[RequestArguments.Login] =
    XtbRequest("login", None, RequestArguments.Login(userId, password))

  def enterMarket(sessionId: String, pair: CurrencyPair, order: TradeOrder.Enter): XtbRequest[RequestArguments.Trade] =
    XtbRequest(
      "tradeTransaction",
      Some(sessionId),
      RequestArguments.Trade(
        `type` = 0,
        cmd = Some(if (order.position == TradeOrder.Position.Buy) 0 else 1),
        symbol = s"${pair.base}${pair.quote}",
        customComment = s"Currex - ${TradeOrder.Position.Buy.toString} $pair",
        offset = order.trailingStopLoss,
        price = Some(BigDecimal(0.1)),
        sl = order.stopLoss,
        tp = order.takeProfit,
        volume = Some(order.volume)
      )
    )

  def exitMarket(sessionId: String, pair: CurrencyPair): XtbRequest[RequestArguments.Trade] =
    XtbRequest(
      "tradeTransaction",
      Some(sessionId),
      RequestArguments.Trade(
        `type` = 2,
        symbol = s"${pair.base}${pair.quote}",
        customComment = s"Currex - Close $pair",
        cmd = None,
        offset = None,
        price = None,
        sl = None,
        tp = None,
        volume = None
      )
    )
}
