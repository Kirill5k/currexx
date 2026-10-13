package currexx.backtest.optimizer.reporting

import currexx.backtest.optimizer.strategy.{RuleList, RuleParameter, RuleParameterBounds, RuleParameterPath, RulePath, RuleTarget}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.ValueRole
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ListMap
import scala.concurrent.duration.*

class RuleBoundsRendererSpec extends AnyWordSpec with Matchers {
  "RuleBoundsRenderer" should {
    "retain named and explicit bounds, template snapshots and nested paths in stable target order" in {
      val template = TradeStrategy(
        List(Rule(TradeAction.OpenLong, Rule.Condition.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50.0))),
        List(Rule(TradeAction.ClosePosition, Rule.Condition.Not(Rule.Condition.PositionOpenFor(2.hours))))
      )
      val level = RuleTarget.At(RuleParameterPath(RulePath(RuleList.Open, 0), RuleParameter.Level), template)
      val duration = RuleTarget.At(RuleParameterPath(RulePath(RuleList.Close, 0, Vector(0)), RuleParameter.Duration), template)
      val bounds = RuleParameterBounds(
        numeric = ListMap(
          RuleTarget.AtrStop -> RuleParameterBounds.DoubleRange(0.5, 20.0, 0.5),
          level -> RuleParameterBounds.DoubleRange(20.0, 80.0, 1.0)
        ),
        durations = ListMap(
          RuleTarget.TrendAge -> RuleParameterBounds.DurationRange(0.hours, 2.days, 1.hour),
          RuleTarget.HoldingTime -> RuleParameterBounds.DurationRange(1.hour, 12.hours, 1.hour),
          duration -> RuleParameterBounds.DurationRange(30.minutes, 4.hours, 30.minutes)
        )
      )
      val expected = parse(s"""{
        "numeric": [
          {"target": {"kind": "At", "path": {"ruleList": "Open", "ruleIndex": 0, "conditions": [], "parameter": "Level"},
            "template": ${template.asJson.noSpaces}}, "range": {"min": 20.0, "max": 80.0, "step": 1.0}},
          {"target": {"kind": "AtrStop"}, "range": {"min": 0.5, "max": 20.0, "step": 0.5}}
        ],
        "durations": [
          {"target": {"kind": "At", "path": {"ruleList": "Close", "ruleIndex": 0, "conditions": [0], "parameter": "Duration"},
            "template": ${template.asJson.noSpaces}}, "min": "30 minutes", "max": "4 hours", "step": "30 minutes"},
          {"target": {"kind": "HoldingTime"}, "min": "1 hour", "max": "12 hours", "step": "1 hour"},
          {"target": {"kind": "TrendAge"}, "min": "0 hours", "max": "2 days", "step": "1 hour"}
        ]
      }""")
      val rendered = RuleBoundsRenderer.json(bounds)
      Right(rendered) mustBe expected
      RuleBoundsRenderer.json(bounds.copy(
        numeric = ListMap.from(bounds.numeric.toList.reverse),
        durations = ListMap.from(bounds.durations.toList.reverse)
      )).noSpaces mustBe rendered.noSpaces
    }

    "retain both empty bounds collections" in {
      Right(RuleBoundsRenderer.json(RuleParameterBounds())) mustBe parse("""{"numeric": [], "durations": []}""")
    }
  }
}
