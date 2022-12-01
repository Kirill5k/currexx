package currexx.core.fixtures

import currexx.clients.broker.BrokerParameters
import currexx.core.settings.{GlobalSettings, SignalParameters, TradeParameters}
import currexx.core.signal.{SignalSettings, TriggerFrequency}
import currexx.core.trade.{TradeSettings, TradeStrategy, TradingParameters}

object Settings {

  lazy val signal = SignalParameters(TriggerFrequency.OncePerDay, List(Markets.trendChangeDetection))
  lazy val trade = TradeParameters(
    TradeStrategy.Disabled,
    BrokerParameters.Vindaloo("1"),
    TradingParameters(BigDecimal(0.1)),
    Some("test")
  )

  lazy val global = GlobalSettings(Users.uid, Some(signal), Some(trade))
}
