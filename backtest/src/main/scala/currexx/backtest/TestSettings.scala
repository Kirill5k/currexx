package currexx.backtest

import currexx.clients.broker.BrokerParameters
import currexx.domain.user.UserId
import currexx.core.market.MarketState
import currexx.core.signal.{SignalSettings, TriggerFrequency}
import currexx.core.trade.{TradeSettings, TradeStrategy, TradingParameters}
import currexx.domain.market.{CurrencyPair, Indicator, ValueSource, ValueTransformation}
import mongo4cats.bson.ObjectId
import squants.market.{EUR, GBP}

final case class TestSettings(
    userId: UserId,
    currencyPair: CurrencyPair,
    marketState: MarketState,
    signal: SignalSettings,
    trade: TradeSettings
)

object TestSettings:
  def make(currencyPair: CurrencyPair, indicators: Indicator*): TestSettings = {
    val userId       = UserId(ObjectId.get)
    TestSettings(
      userId = userId,
      currencyPair = currencyPair,
      marketState = MarketState(userId, currencyPair, None, None, Map.empty, None),
      signal = SignalSettings(userId, TriggerFrequency.OncePerDay, indicators.toList),
      trade = TradeSettings(userId, TradeStrategy.TrendChangeAggressive, BrokerParameters.Vindaloo("1"), TradingParameters(BigDecimal(0.1)))
    )
  }
