package currexx.core.fixtures

import currexx.clients.broker.BrokerParameters
import currexx.core.settings.{GlobalSettings, SignalSettings, TradeSettings, TradingParameters}
import currexx.core.trade.TradeStrategy

object Settings {

  lazy val signal = SignalSettings(List(Signals.trendChangeDetection))

  lazy val trade = TradeSettings(
    TradeStrategy.Disabled,
    BrokerParameters.Xtb("user1", "password", demo = true),
    TradingParameters(BigDecimal(0.1))
  )

  lazy val global = GlobalSettings(Users.uid, Some(signal), Some(trade), Some("test"))
}
