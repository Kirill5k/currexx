package currexx.core.fixtures

import currexx.clients.broker.BrokerParameters
import currexx.domain.market.{OpenedTradeOrder, OrderExecution, TradeOrder}
import currexx.core.trade.TradeOrderPlacement

import java.time.Instant
import java.time.temporal.ChronoField

object Trades {
  lazy val ts = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)

  lazy val broker = BrokerParameters.Oanda("key", true, "account")

  lazy val order = TradeOrderPlacement(
    Users.uid,
    TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, Markets.priceRange.close, BigDecimal(0.1)),
    broker,
    ts
  )

  lazy val execution = OrderExecution(
    price = Markets.priceRange.close + BigDecimal("0.0002"),
    time = ts.plusSeconds(1),
    volume = BigDecimal(0.1),
    orderId = "42",
    transactionId = "43",
    tradeIds = List("43")
  )

  lazy val openedOrder = OpenedTradeOrder(
    currencyPair = Markets.gbpeur,
    position = TradeOrder.Position.Buy,
    openPrice = Markets.priceRange.close,
    currentPrice = Markets.priceRange.close,
    volume = BigDecimal(0.1),
    profit = BigDecimal(100)
  )
}
