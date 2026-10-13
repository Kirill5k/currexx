package currexx.backtest.optimizer.reporting

import currexx.backtest.TestStrategy
import currexx.backtest.optimizer.OptimisationTarget
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation}
import kirill5k.common.cats.test.IOWordSpec

class CandidateDisplaySpec extends IOWordSpec {
  "CandidateDisplay" should {
    "show only the searched components for each target" in {
      val candidate = TestStrategy(
        Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(10)),
        TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil)
      )
      CandidateDisplay.forTarget(OptimisationTarget.Indicator)(candidate) mustBe candidate.indicator.toString
      CandidateDisplay.forTarget(OptimisationTarget.Strategy)(candidate) mustBe candidate.rules.toString
      CandidateDisplay.forTarget(OptimisationTarget.Both)(candidate) mustBe candidate.toString
    }
  }
}
