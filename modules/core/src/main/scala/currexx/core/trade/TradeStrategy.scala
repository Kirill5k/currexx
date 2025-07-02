package currexx.core.trade

import currexx.core.market.MarketState
import currexx.domain.types.EnumType

object TradeStrategy extends EnumType[TradeStrategy](() => TradeStrategy.values)
enum TradeStrategy:
  case Disabled

trait TradeStrategyExecutor:
  def analyze(state: MarketState): Option[TradeStrategyExecutor.Decision]

object TradeStrategyExecutor {
  enum Decision:
    case Buy, Sell, Close

  private case object Disabled extends TradeStrategyExecutor:
    def analyze(state: MarketState): Option[TradeStrategyExecutor.Decision] = None

  def get(strategy: TradeStrategy): TradeStrategyExecutor = Disabled
}
