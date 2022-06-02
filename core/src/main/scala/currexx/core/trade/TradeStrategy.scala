package currexx.core.trade

import currexx.core.market.MarketState
import currexx.domain.market.{Condition, TradeOrder, Trend}
import currexx.domain.market.v2.Indicator
import io.circe.{Decoder, Encoder}

enum TradeStrategy(val name: String):
  case Disabled    extends TradeStrategy("disabled")
  case TrendChange extends TradeStrategy("trend-change")

object TradeStrategy:
  inline given Encoder[TradeStrategy] = Encoder[String].contramap(_.name)
  inline given Decoder[TradeStrategy] =
    Decoder[String].emap(s => TradeStrategy.values.find(_.name == s).toRight(s"Unrecognized strategy $s"))

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
        case Indicator.TrendChangeDetection =>
          state.signals.getOrElse(trigger.kind, Nil).headOption.map(_.condition).collect {
            case Condition.TrendDirectionChange(Trend.`Downward`, Trend.Consolidation) => Decision.Close
            case Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation)   => Decision.Close
            case Condition.TrendDirectionChange(_, Trend.Upward) if !state.buying    => Decision.Buy
            case Condition.TrendDirectionChange(_, Trend.Downward) if !state.selling => Decision.Sell
          }
        case _ => None

  def get(strategy: TradeStrategy): TradeStrategyExecutor =
    strategy match
      case TradeStrategy.Disabled    => Disabled
      case TradeStrategy.TrendChange => TrendChange

  extension (state: MarketState)
    def hasPosition: Boolean = state.currentPosition.isDefined
    def buying: Boolean      = state.currentPosition.map(_.position).contains(TradeOrder.Position.Buy)
    def selling: Boolean     = state.currentPosition.map(_.position).contains(TradeOrder.Position.Sell)
}
