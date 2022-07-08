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

  final case class TradeTransInfo(
      cmd: Option[Int],
      `type`: Int,
      symbol: String,
      customComment: String,
      volume: Option[BigDecimal] = None,
      price: Option[BigDecimal] = None,
      sl: Option[BigDecimal] = None,
      tp: Option[BigDecimal] = None,
      offset: Option[BigDecimal] = None,
      order: Option[Long] = None
  ) extends RequestArguments
      derives Codec.AsObject

  final case class Trade(tradeTransInfo: TradeTransInfo) extends RequestArguments derives Codec.AsObject

final case class XtbRequest[A <: RequestArguments](
    command: String,
    streamSessionId: Option[String],
    arguments: A
) derives Codec.AsObject

object XtbRequest {
  def login(userId: String, password: String): XtbRequest[RequestArguments.Login] =
    XtbRequest("login", None, RequestArguments.Login(userId, password))

  def trade(sessionId: String, pair: CurrencyPair, order: TradeOrder): XtbRequest[RequestArguments.Trade] =
    XtbRequest(
      "tradeTransaction",
      Some(sessionId),
      RequestArguments.Trade(
        tradeTransInfo = order match
          case TradeOrder.Exit         => exitMarket(pair)
          case enter: TradeOrder.Enter => enterMarket(pair, enter)
      )
    )

  private def enterMarket(pair: CurrencyPair, order: TradeOrder.Enter): RequestArguments.TradeTransInfo =
    RequestArguments.TradeTransInfo(
      `type` = 0,
      cmd = Some(if (order.position == TradeOrder.Position.Buy) 0 else 1),
      symbol = s"${pair.base}${pair.quote}",
      customComment = s"Currexx - ${TradeOrder.Position.Buy.toString} $pair",
      offset = order.trailingStopLoss,
      price = Some(BigDecimal(0.1)),
      sl = order.stopLoss,
      tp = order.takeProfit,
      volume = Some(order.volume)
    )

  private def exitMarket(pair: CurrencyPair): RequestArguments.TradeTransInfo =
    RequestArguments.TradeTransInfo(
      `type` = 2,
      symbol = s"${pair.base}${pair.quote}",
      customComment = s"Currexx - Close $pair",
      cmd = Some(0),
      order = Some(403108161),
      price = Some(BigDecimal(0)),
      volume = Some(BigDecimal(0)),
    )
}
