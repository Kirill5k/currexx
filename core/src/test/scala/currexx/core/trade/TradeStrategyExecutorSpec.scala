package currexx.core.trade

import currexx.core.fixtures.Markets
import currexx.core.fixtures.Markets
import currexx.core.market.IndicatorState
import currexx.domain.market.{Condition, Indicator, Trend, TradeOrder}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

class TradeStrategyExecutorSpec extends AnyWordSpec with Matchers {

  "A TradeStrategyExecutor.HMABasic" should {

    "make Buy decision when trend changes to Upward" in {
      val condition = Condition.TrendDirectionChange(Trend.Consolidation, Trend.Upward)
      val state     = Markets.state.copy(
        signals = Map(Indicator.HMA -> List(IndicatorState(condition, Markets.ts))),
        currentPosition = None
      )

      TradeStrategyExecutor.get(TradeStrategy.HMABasic).analyze(state, Indicator.HMA) mustBe Some(TradeStrategyExecutor.Decision.Buy)
    }

    "make Sell decision when trend changes to Downward" in {
      val condition = Condition.TrendDirectionChange(Trend.Consolidation, Trend.Downward)
      val state     = Markets.state.copy(signals = Map(Indicator.HMA -> List(IndicatorState(condition, Markets.ts))))

      TradeStrategyExecutor.get(TradeStrategy.HMABasic).analyze(state, Indicator.HMA) mustBe Some(TradeStrategyExecutor.Decision.Sell)
    }

    "make Close decision when trend goes into Consolidation" in {
      val condition = Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation)
      val state     = Markets.state.copy(signals = Map(Indicator.HMA -> List(IndicatorState(condition, Markets.ts))))

      TradeStrategyExecutor.get(TradeStrategy.HMABasic).analyze(state, Indicator.HMA) mustBe Some(TradeStrategyExecutor.Decision.Close)
    }

    "not do anything when trade change was triggered by another indicator" in {
      val condition = Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation)
      val state     = Markets.state.copy(signals = Map(Indicator.HMA -> List(IndicatorState(condition, Markets.ts))))

      TradeStrategyExecutor.get(TradeStrategy.HMABasic).analyze(state, Indicator.MACD) mustBe None
    }

    "not do anything when there are no relevant signals in state" in {
      TradeStrategyExecutor.get(TradeStrategy.HMABasic).analyze(Markets.stateWithSignal, Indicator.HMA) mustBe None
    }
  }
}
