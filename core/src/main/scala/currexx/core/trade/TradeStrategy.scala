package currexx.core.trade

import currexx.core.market.MarketState
import currexx.domain.market.{Condition, Indicator, TradeOrder, Trend}
import io.circe.{Decoder, Encoder}

enum TradeStrategy(val name: String):
  case Disabled extends TradeStrategy("disabled")
  case HMABasic extends TradeStrategy("hma-basic")

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

  private case object HMABasic extends TradeStrategyExecutor:
    def analyze(state: MarketState, trigger: Indicator): Option[TradeStrategyExecutor.Decision] =
      trigger match
        case Indicator.HMA =>
          state.signals.getOrElse(trigger, Nil).headOption.map(_.condition).collect {
            case Condition.TrendDirectionChange(_, Trend.Consolidation) => Decision.Close
            case Condition.TrendDirectionChange(_, Trend.Upward)        => Decision.Buy
            case Condition.TrendDirectionChange(_, Trend.Downward)      => Decision.Sell
          }
        case _ => None

  def get(strategy: TradeStrategy): TradeStrategyExecutor =
    strategy match
      case TradeStrategy.Disabled => Disabled
      case TradeStrategy.HMABasic => HMABasic

  extension (state: MarketState)
    def hasPosition: Boolean = state.currentPosition.isDefined
    def buying: Boolean      = state.currentPosition.contains(TradeOrder.Position.Buy)
    def selling: Boolean     = state.currentPosition.contains(TradeOrder.Position.Sell)
}
