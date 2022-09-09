package currexx.core.trade

import currexx.core.market.MarketState
import currexx.domain.market.{Condition, Indicator, TradeOrder, Trend}
import io.circe.{Decoder, Encoder}

enum TradeStrategy(val name: String):
  case Disabled              extends TradeStrategy("disabled")
  case TrendChange           extends TradeStrategy("trend-change")
  case TrendChangeAggressive extends TradeStrategy("trend-change-aggressive")

object TradeStrategy:
  def from(name: String): Option[TradeStrategy] = TradeStrategy.values.find(_.name == name)

  inline given Encoder[TradeStrategy] = Encoder[String].contramap(_.name)
  inline given Decoder[TradeStrategy] = Decoder[String].emap(s => from(s).toRight(s"Unrecognized strategy $s"))

trait TradeStrategyExecutor:
  def analyze(state: MarketState, trigger: Indicator): Option[TradeStrategyExecutor.Decision]

object TradeStrategyExecutor {
  enum Decision:
    case Buy, Sell, Close

  private case object Disabled extends TradeStrategyExecutor:
    def analyze(state: MarketState, trigger: Indicator): Option[TradeStrategyExecutor.Decision] = None

  private case object TrendChange extends TradeStrategyExecutor:
    def analyze(state: MarketState, trigger: Indicator): Option[TradeStrategyExecutor.Decision] =
      trigger match
        case _: Indicator.TrendChangeDetection =>
          state.signals.getOrElse(trigger.kind, Nil).headOption.map(_.condition).collect {
            case Condition.TrendDirectionChange(Trend.Downward, Trend.Consolidation, _) => Decision.Close
            case Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation, _)   => Decision.Close
            case Condition.TrendDirectionChange(_, Trend.Upward, _) if !state.buying    => Decision.Buy
            case Condition.TrendDirectionChange(_, Trend.Downward, _) if !state.selling => Decision.Sell
          }
        case _ => None

  private case object TrendChangeAggressive extends TradeStrategyExecutor:
    def analyze(state: MarketState, trigger: Indicator): Option[TradeStrategyExecutor.Decision] =
      trigger match
        case _: Indicator.TrendChangeDetection =>
          state.signals.getOrElse(trigger.kind, Nil).headOption.map(_.condition).collect {
            case Condition.TrendDirectionChange(Trend.Downward, Trend.Consolidation, _) => Decision.Buy
            case Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation, _)   => Decision.Sell
            case Condition.TrendDirectionChange(_, Trend.Upward, _) if !state.buying    => Decision.Buy
            case Condition.TrendDirectionChange(_, Trend.Downward, _) if !state.selling => Decision.Sell
          }
        case _ => None

  def get(strategy: TradeStrategy): TradeStrategyExecutor =
    strategy match
      case TradeStrategy.Disabled              => Disabled
      case TradeStrategy.TrendChange           => TrendChange
      case TradeStrategy.TrendChangeAggressive => TrendChangeAggressive

  extension (state: MarketState)
    def buying: Boolean  = state.currentPosition.map(_.position).contains(TradeOrder.Position.Buy)
    def selling: Boolean = state.currentPosition.map(_.position).contains(TradeOrder.Position.Sell)
}
