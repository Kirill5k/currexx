package currexx.core.fixtures

import currexx.clients.broker.BrokerParameters
import currexx.core.trade.{TradeSettings, TradingParameters}

object Trades {
  lazy val settings = TradeSettings(Users.uid, BrokerParameters.Vindaloo("1"), TradingParameters(BigDecimal(0.1), None, None, None))
}
