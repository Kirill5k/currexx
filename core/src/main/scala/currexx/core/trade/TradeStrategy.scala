package currexx.core.trade

import cats.syntax.applicative.*
import currexx.core.market.MarketState
import currexx.domain.market.{Condition, Direction, IndicatorKind, TradeOrder}
import currexx.domain.types.EnumType

object TradeStrategy extends EnumType[TradeStrategy](() => TradeStrategy.values)
enum TradeStrategy:
  case Disabled, TrendChange, TrendChangeAggressive, LinesCrossing, ThresholdCrossing

trait TradeStrategyExecutor:
  def analyze(state: MarketState, triggers: List[IndicatorKind]): Option[TradeStrategyExecutor.Decision]

object TradeStrategyExecutor {
  enum Decision:
    case Buy, Sell, Close

  private case object Disabled extends TradeStrategyExecutor:
    def analyze(state: MarketState, triggers: List[IndicatorKind]): Option[TradeStrategyExecutor.Decision] = None

  private case object TrendChange extends TradeStrategyExecutor:
    def analyze(state: MarketState, triggers: List[IndicatorKind]): Option[TradeStrategyExecutor.Decision] =
      triggers
        .find(_ == IndicatorKind.TrendChangeDetection)
        .flatMap { tcd =>
          state.signals.getOrElse(tcd, Nil).headOption.map(_.condition).collect {
            case Condition.TrendDirectionChange(Direction.Downward, Direction.Still, _)     => Decision.Close
            case Condition.TrendDirectionChange(Direction.Upward, Direction.Still, _)       => Decision.Close
            case Condition.TrendDirectionChange(_, Direction.Upward, _) if !state.buying    => Decision.Buy
            case Condition.TrendDirectionChange(_, Direction.Downward, _) if !state.selling => Decision.Sell
          }
        }

  private case object TrendChangeAggressive extends TradeStrategyExecutor:
    def analyze(state: MarketState, triggers: List[IndicatorKind]): Option[TradeStrategyExecutor.Decision] =
      triggers
        .find(_ == IndicatorKind.TrendChangeDetection)
        .flatMap { tcd =>
          state.signals.getOrElse(tcd, Nil).headOption.map(_.condition).collect {
            case Condition.TrendDirectionChange(Direction.Downward, Direction.Still, _) if !state.buying => Decision.Buy
            case Condition.TrendDirectionChange(Direction.Upward, Direction.Still, _) if !state.selling  => Decision.Sell
            case Condition.TrendDirectionChange(_, Direction.Upward, _) if !state.buying                 => Decision.Buy
            case Condition.TrendDirectionChange(_, Direction.Downward, _) if !state.selling              => Decision.Sell
          }
        }

  private case object LinesCrossing extends TradeStrategyExecutor:
    def analyze(state: MarketState, triggers: List[IndicatorKind]): Option[TradeStrategyExecutor.Decision] =
      triggers
        .find(_ == IndicatorKind.LinesCrossing)
        .flatMap { lc =>
          state.signals.getOrElse(lc, Nil).headOption.map(_.condition).collect {
            case Condition.LinesCrossing(Direction.Upward)   => Decision.Sell
            case Condition.LinesCrossing(Direction.Downward) => Decision.Buy
          }
        }

  private case object ThresholdCrossing extends TradeStrategyExecutor:
    def analyze(state: MarketState, triggers: List[IndicatorKind]): Option[TradeStrategyExecutor.Decision] =
      triggers
        .find(_ == IndicatorKind.ThresholdCrossing)
        .flatMap { tc =>
          state.signals.getOrElse(tc, Nil).headOption.map(_.condition).collect {
            case _: Condition.AboveThreshold => Decision.Sell
            case _: Condition.BelowThreshold => Decision.Buy
          }
        }

  def get(strategy: TradeStrategy): TradeStrategyExecutor =
    strategy match
      case TradeStrategy.Disabled              => Disabled
      case TradeStrategy.TrendChange           => TrendChange
      case TradeStrategy.TrendChangeAggressive => TrendChangeAggressive
      case TradeStrategy.LinesCrossing         => LinesCrossing
      case TradeStrategy.ThresholdCrossing     => ThresholdCrossing

  extension (state: MarketState)
    def buying: Boolean  = state.currentPosition.map(_.position).contains(TradeOrder.Position.Buy)
    def selling: Boolean = state.currentPosition.map(_.position).contains(TradeOrder.Position.Sell)
}
