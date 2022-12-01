package currexx.core.fixtures

import currexx.clients.broker.BrokerParameters
import currexx.core.settings.{GlobalSettings, SignalSettings, TradeSettings, TradingParameters, TriggerFrequency}
import currexx.core.trade.TradeStrategy

object Settings {

  lazy val signal = SignalSettings(TriggerFrequency.OncePerDay, List(Markets.trendChangeDetection))
  lazy val trade = TradeSettings(
    TradeStrategy.Disabled,
    BrokerParameters.Vindaloo("1"),
    TradingParameters(BigDecimal(0.1)),
    Some("test")
  )

  lazy val global = GlobalSettings(Users.uid, Some(signal), Some(trade))
}
