package currexx.core.fixtures

import currexx.clients.broker.BrokerParameters
import currexx.domain.market.TradeOrder
import currexx.core.trade.{TradeOrderPlacement, TradeSettings, TradeStrategy, TradingParameters}

import java.time.Instant
import java.time.temporal.ChronoField

object Trades {
  lazy val ts = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)

  lazy val broker = BrokerParameters.Vindaloo("1")

  lazy val settings = TradeSettings(
    Users.uid,
    TradeStrategy.Disabled,
    BrokerParameters.Vindaloo("1"),
    TradingParameters(BigDecimal(0.1)),
    Some("test")
  )

  lazy val order = TradeOrderPlacement(
    Users.uid,
    Markets.gbpeur,
    TradeOrder.Enter(TradeOrder.Position.Buy, BigDecimal(0.1), Some(BigDecimal(25)), None, None),
    broker,
    Markets.priceRange,
    ts
  )
}
