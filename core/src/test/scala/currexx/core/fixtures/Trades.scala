package currexx.core.fixtures

import currexx.clients.broker.BrokerParameters
import currexx.domain.market.{OpenedTradeOrder, TradeOrder}
import currexx.core.trade.{TradeOrderPlacement, TradeSettings, TradeStrategy, TradingParameters}

import java.time.Instant
import java.time.temporal.ChronoField

object Trades {
  lazy val ts = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)

  lazy val broker = BrokerParameters.Vindaloo("1")

  lazy val order = TradeOrderPlacement(
    Users.uid,
    TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, Markets.priceRange.close, BigDecimal(0.1)),
    broker,
    ts
  )
  
  lazy val openedOrder = OpenedTradeOrder(
    Markets.gbpeur,
    TradeOrder.Position.Buy,
    Markets.priceRange.close,
    Markets.priceRange.close,
    ts,
    BigDecimal(0.1),
    BigDecimal(100)
  )
}
