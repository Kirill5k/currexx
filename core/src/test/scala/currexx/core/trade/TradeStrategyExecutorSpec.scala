package currexx.core.trade

import currexx.core.fixtures.Markets
import currexx.core.fixtures.Markets
import currexx.core.market.{IndicatorState, PositionState}
import currexx.domain.market.{Condition, Indicator, IndicatorKind, TradeOrder, Trend}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

class TradeStrategyExecutorSpec extends AnyWordSpec with Matchers {

  "A TradeStrategyExecutor.TrendChange" should {

    val indicator = Markets.trendChangeDetection
    val kind      = indicator.kind

    "make Buy decision when trend changes to Upward" in {
      val condition = Condition.TrendDirectionChange(Trend.Consolidation, Trend.Upward)
      val state = Markets.state.copy(
        signals = Map(kind -> List(IndicatorState(condition, Markets.ts, indicator))),
        currentPosition = None
      )

      TradeStrategyExecutor.get(TradeStrategy.TrendChange).analyze(state, List(kind)) mustBe Some(TradeStrategyExecutor.Decision.Buy)
    }

    "make Sell decision when trend changes to Downward" in {
      val condition = Condition.TrendDirectionChange(Trend.Consolidation, Trend.Downward)
      val state     = Markets.state.copy(signals = Map(kind -> List(IndicatorState(condition, Markets.ts, indicator))))

      TradeStrategyExecutor.get(TradeStrategy.TrendChange).analyze(state, List(kind)) mustBe Some(TradeStrategyExecutor.Decision.Sell)
    }

    "make Close decision when trend goes into Consolidation from Upward" in {
      val condition = Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation)
      val state     = Markets.state.copy(signals = Map(kind -> List(IndicatorState(condition, Markets.ts, indicator))))

      TradeStrategyExecutor.get(TradeStrategy.TrendChange).analyze(state, List(kind)) mustBe Some(TradeStrategyExecutor.Decision.Close)
    }

    "not do anything when there are no relevant signals in state" in {
      TradeStrategyExecutor.get(TradeStrategy.TrendChange).analyze(Markets.state, List(kind)) mustBe None
    }

    "not do anything if state already has opened position" in {
      val condition = Condition.TrendDirectionChange(Trend.Consolidation, Trend.Downward)
      val state = Markets.state.copy(
        signals = Map(kind -> List(IndicatorState(condition, Markets.ts, indicator))),
        currentPosition = Some(PositionState(TradeOrder.Position.Sell, Markets.ts, Markets.priceRange.close))
      )

      TradeStrategyExecutor.get(TradeStrategy.TrendChange).analyze(state, List(kind)) mustBe None
    }
  }
}
