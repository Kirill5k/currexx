package currexx.backtest.optimizer.reporting

import currexx.backtest.TestStrategy
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.concurrent.duration.*

class CandidateParametersSpec extends IOWordSpec {
  private def candidate(trendAge: FiniteDuration, holdingTime: FiniteDuration, threshold: Double = 2.0): TestStrategy =
    TestStrategy(
      Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(10)),
      TradeStrategy(
        List(Rule(TradeAction.OpenLong, Rule.Condition.TrendActiveFor(trendAge))),
        List(
          Rule(
            TradeAction.ClosePosition,
            Rule.Condition.allOf(
              Rule.Condition.Not(Rule.Condition.PositionOpenFor(holdingTime)),
              Rule.Condition.VelocityIsBelow(threshold)
            )
          )
        )
      )
    )

  "CandidateParameters" should {
    "omit equivalent durations even when zero uses different units" in {
      CandidateParameters.changes(candidate(0.hours, 0.minutes), candidate(0.nanos, 0.days)) mustBe Nil
      CandidateParameters.changes(candidate(1.hour, 60.minutes), candidate(60.minutes, 3600.seconds)) mustBe Nil
    }

    "retain true changes beside equivalent durations in nested conditions" in {
      val changes = CandidateParameters.changes(candidate(0.hours, 1.hour), candidate(0.nanos, 2.hours, 3.0))
      changes must have size 2
      changes.count(_.contains(".duration:")) mustBe 1
      changes must contain("rules.closeRules[0].conditions.conditions[0].condition.duration: \"1 hour\" -> \"2 hours\"")
      changes must contain("rules.closeRules[0].conditions.conditions[1].threshold: 2.0 -> 3.0")
    }
  }
}
