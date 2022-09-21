package currexx.backtest

import currexx.clients.broker.BrokerParameters
import currexx.domain.user.UserId
import currexx.core.market.MarketState
import currexx.core.signal.{SignalSettings, TriggerFrequency}
import currexx.core.trade.{TradeSettings, TradeStrategy, TradingParameters}
import currexx.domain.market.{CurrencyPair, Indicator, ValueSource, ValueTransformation}
import mongo4cats.bson.ObjectId
import currexx.domain.market.Currency.{EUR, GBP}

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
      marketState = MarketState(userId, currencyPair, None, None, Map.empty, None, None),
      signal = SignalSettings(userId, TriggerFrequency.OncePerDay, indicators),
      trade = TradeSettings(userId, strategy, BrokerParameters.Vindaloo("1"), TradingParameters(BigDecimal(0.1)), None)
    )
  }
