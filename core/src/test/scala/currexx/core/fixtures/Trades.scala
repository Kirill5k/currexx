package currexx.core.fixtures

import currexx.clients.broker.BrokerParameters
import currexx.core.trade.{TradeSettings, TradingParameters, TradeStrategy}

object Trades {
  lazy val settings = TradeSettings(
    Users.uid,
    TradeStrategy.Simple,
    BrokerParameters.Vindaloo("1"),
    TradingParameters(BigDecimal(0.1), None, None, None)
  )
}
