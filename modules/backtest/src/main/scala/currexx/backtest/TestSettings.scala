package currexx.backtest

import currexx.clients.broker.BrokerParameters
import currexx.core.market.{MarketProfile, MarketState}
import currexx.core.settings.{SignalSettings, TradeSettings, TradingParameters}
import currexx.core.trade.TradeStrategy
import currexx.domain.market.CurrencyPair
import currexx.domain.signal.Indicator
import currexx.domain.user.UserId
import mongo4cats.bson.ObjectId

import java.time.Instant

final case class TestSettings(
    userId: UserId,
    marketState: MarketState,
    signal: SignalSettings,
    trade: TradeSettings
)

object TestSettings:
  def make(
      currencyPair: CurrencyPair,
      strategy: TradeStrategy,
      indicators: List[Indicator]
  ): TestSettings =
    val userId = UserId(ObjectId.gen)
    val now    = Instant.now()
    TestSettings(
      userId = userId,
      marketState = MarketState(userId, currencyPair, None, MarketProfile(), now, now),
      signal = SignalSettings(indicators),
      trade = TradeSettings(strategy, BrokerParameters.Oanda("key", true, "account"), TradingParameters(BigDecimal(0.1)))
    )
