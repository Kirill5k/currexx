package currexx.core.fixtures

import currexx.clients.broker.BrokerParameters
import currexx.domain.market.{OpenedTradeOrder, TradeOrder}
import currexx.core.trade.TradeOrderPlacement

import java.time.Instant
import java.time.temporal.ChronoField

object Trades {
  lazy val ts = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)

  lazy val broker = BrokerParameters.Xtb("user1", "password", demo = true)

  lazy val order = TradeOrderPlacement(
    Users.uid,
    TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, BigDecimal(0.1)),
    broker,
    ts
  )

  lazy val openedOrder = OpenedTradeOrder(
    currencyPair = Markets.gbpeur,
    position = TradeOrder.Position.Buy,
    openPrice = Markets.priceRange.close,
    volume = BigDecimal(0.1),
    profit = BigDecimal(100)
  )
}
