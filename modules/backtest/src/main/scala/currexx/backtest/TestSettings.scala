package currexx.backtest

import currexx.clients.broker.BrokerParameters
import currexx.core.market.MarketState
import currexx.core.settings.{SignalSettings, TradeSettings, TradingParameters, TriggerFrequency}
import currexx.core.trade.TradeStrategy
import currexx.domain.market.{CurrencyPair, Indicator}
import currexx.domain.user.UserId
import mongo4cats.bson.ObjectId

final case class TestSettings(
    userId: UserId,
    currencyPair: CurrencyPair,
    marketState: MarketState,
    signal: SignalSettings,
    trade: TradeSettings
)

object TestSettings:
  def make(
      currencyPair: CurrencyPair,
      strategy: TradeStrategy,
      indicators: List[Indicator]
  ): TestSettings = {
    val userId = UserId(ObjectId.gen)
    TestSettings(
      userId = userId,
      currencyPair = currencyPair,
      marketState = MarketState(userId, currencyPair, None, Map.empty, None, None),
      signal = SignalSettings(TriggerFrequency.OncePerDay, indicators),
      trade = TradeSettings(strategy, BrokerParameters.Vindaloo("1"), TradingParameters(BigDecimal(0.1)))
    )
  }
