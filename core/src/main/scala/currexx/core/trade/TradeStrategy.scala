package currexx.core.trade

import currexx.core.market.MarketState
import currexx.domain.market.Indicator

sealed trait TradeStrategy:
  def analyze(state: MarketState, trigger: Indicator): Option[TradeStrategy.Outcome]

object TradeStrategy {
  enum Outcome:
    case Buy, Sell, Close
}