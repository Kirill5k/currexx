package currexx.clients.broker.xtb

import currexx.domain.market.{CurrencyPair, TradeOrder}
import io.circe.Codec

import java.time.Instant

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
      volume: Option[BigDecimal] = None,
      sl: Option[BigDecimal] = None,
      tp: Option[BigDecimal] = None,
      offset: Option[BigDecimal] = None,
      order: Option[Long] = None,
      expiration: Option[Long] = None
  ) extends RequestArguments
      derives Codec.AsObject

  final case class Trade(tradeTransInfo: TradeTransInfo) extends RequestArguments derives Codec.AsObject

  final case class TickPrice(level: Int, symbols: List[String], timestamp: Long) extends RequestArguments derives Codec.AsObject

final case class XtbRequest[A <: RequestArguments](
    command: String,
    streamSessionId: Option[String],
    arguments: A
) derives Codec.AsObject

object XtbRequest {
  def login(userId: String, password: String): XtbRequest[RequestArguments.Login] =
    XtbRequest("login", None, RequestArguments.Login(userId, password))

  def trade(sessionId: String, cp: CurrencyPair, order: TradeOrder, price: BigDecimal): XtbRequest[RequestArguments.Trade] =
    XtbRequest(
      "tradeTransaction",
      Some(sessionId),
      RequestArguments.Trade(
        tradeTransInfo = order match
          case TradeOrder.Exit         => exitMarket(cp, price)
          case enter: TradeOrder.Enter => enterMarket(cp, enter, price)
      )
    )

  def tickPrice(sessionId: String, cp: CurrencyPair): XtbRequest[RequestArguments.TickPrice] =
    XtbRequest(
      "getTickPrices",
      Some(sessionId),
      RequestArguments.TickPrice(
        level = 0,
        symbols = List(cp.toSymbol),
        timestamp = Instant.now.toEpochMilli
      )
    )

  private def enterMarket(cp: CurrencyPair, order: TradeOrder.Enter, price: BigDecimal): RequestArguments.TradeTransInfo =
    RequestArguments.TradeTransInfo(
      `type` = 0,
      cmd = Some(if (order.position == TradeOrder.Position.Buy) 0 else 1),
      symbol = cp.toSymbol,
      customComment = s"Currexx - ${TradeOrder.Position.Buy.toString} $cp",
      offset = order.trailingStopLoss,
      price = price,
      sl = order.stopLoss,
      tp = order.takeProfit,
      volume = Some(order.volume)
    )

  private def exitMarket(cp: CurrencyPair, price: BigDecimal): RequestArguments.TradeTransInfo =
    RequestArguments.TradeTransInfo(
      `type` = 2,
      price = price,
      symbol = cp.toSymbol,
      customComment = s"Currexx - Close $cp",
      cmd = None
    )
}
