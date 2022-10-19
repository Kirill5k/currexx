package currexx.core.trade

import cats.syntax.applicative.*
import currexx.core.market.MarketState
import currexx.domain.market.{Condition, IndicatorKind, TradeOrder, Trend}
import currexx.domain.types.EnumType

object TradeStrategy extends EnumType[TradeStrategy](() => TradeStrategy.values)
enum TradeStrategy:
  case Disabled, TrendChange, TrendChangeAggressive, TrendChangeWithConfirmation, LinesCrossing

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
            case Condition.TrendDirectionChange(Trend.Downward, Trend.Consolidation, _) => Decision.Close
            case Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation, _)   => Decision.Close
            case Condition.TrendDirectionChange(_, Trend.Upward, _) if !state.buying    => Decision.Buy
            case Condition.TrendDirectionChange(_, Trend.Downward, _) if !state.selling => Decision.Sell
          }
        }

  private case object TrendChangeAggressive extends TradeStrategyExecutor:
    def analyze(state: MarketState, triggers: List[IndicatorKind]): Option[TradeStrategyExecutor.Decision] =
      triggers
        .find(_ == IndicatorKind.TrendChangeDetection)
        .flatMap { tcd =>
          state.signals.getOrElse(tcd, Nil).headOption.map(_.condition).collect {
            case Condition.TrendDirectionChange(Trend.Downward, Trend.Consolidation, _) if !state.buying => Decision.Buy
            case Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation, _) if !state.selling  => Decision.Sell
            case Condition.TrendDirectionChange(_, Trend.Upward, _) if !state.buying                     => Decision.Buy
            case Condition.TrendDirectionChange(_, Trend.Downward, _) if !state.selling                  => Decision.Sell
          }
        }

  private case object TrendChangeWithConfirmation extends TradeStrategyExecutor:
    def analyze(state: MarketState, triggers: List[IndicatorKind]): Option[TradeStrategyExecutor.Decision] = {
      val conditions = triggers.flatMap(t => state.signals.getOrElse(t, Nil).headOption.map(t -> _.condition)).toMap

      (conditions.get(IndicatorKind.TrendChangeDetection), conditions.get(IndicatorKind.ThresholdCrossing)) match
        case (Some(Condition.TrendDirectionChange(Trend.Downward, _, _)), Some(Condition.BelowThreshold(_, _))) =>
          Some(Decision.Buy)
        case (Some(Condition.TrendDirectionChange(Trend.Upward, _, _)), Some(Condition.AboveThreshold(_, _))) =>
          Some(Decision.Sell)
        case (Some(Condition.TrendDirectionChange(Trend.Downward, _, _)), _) =>
          Some(Decision.Close)
        case (Some(Condition.TrendDirectionChange(Trend.Upward, _, _)), _) =>
          Some(Decision.Close)
        case _ => None
    }

  private case object LinesCrossing extends TradeStrategyExecutor:
    def analyze(state: MarketState, triggers: List[IndicatorKind]): Option[TradeStrategyExecutor.Decision] =
      triggers
        .find(_ == IndicatorKind.LinesCrossing)
        .flatMap { ls =>
          state.signals.getOrElse(ls, Nil).headOption.map(_.condition).collect {
            case Condition.CrossingUp   => Decision.Sell
            case Condition.CrossingDown => Decision.Buy
          }
        }

  def get(strategy: TradeStrategy): TradeStrategyExecutor =
    strategy match
      case TradeStrategy.Disabled                    => Disabled
      case TradeStrategy.TrendChange                 => TrendChange
      case TradeStrategy.TrendChangeAggressive       => TrendChangeAggressive
      case TradeStrategy.TrendChangeWithConfirmation => TrendChangeWithConfirmation
      case TradeStrategy.LinesCrossing               => LinesCrossing

  extension (state: MarketState)
    def buying: Boolean  = state.currentPosition.map(_.position).contains(TradeOrder.Position.Buy)
    def selling: Boolean = state.currentPosition.map(_.position).contains(TradeOrder.Position.Sell)
}
