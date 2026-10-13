package currexx.backtest.optimizer.strategy

import currexx.core.trade.Rule
import currexx.domain.signal.{Direction, ValueRole}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.*

class TradeStrategyDistanceSpec extends AnyWordSpec with Matchers {
  import Rule.Condition as C
  import RuleParameterBounds.{DoubleRange, DurationRange}
  import TradeStrategyFixtures.*

  "TradeStrategyDistance" should {
    "count distinct base-value groups separately while counting each repeated group once" in {
      val first  = single(C.AllOf(List(C.TrendActiveFor(3.hours), C.TrendActiveFor(5.hours), C.TrendActiveFor(3.hours))))
      val second = single(C.AllOf(List(C.TrendActiveFor(15.hours), C.TrendActiveFor(5.hours), C.TrendActiveFor(15.hours))))
      val bounds = RuleParameterBounds(durations = Map(RuleTarget.TrendAge -> DurationRange(0.hours, 24.hours, 1.hour)))
      val local  = TradeStrategySearchSpace.forStrategy(first, indicator, bounds).toOption.get
      local.parameterCount mustBe 2
      TradeStrategySearchOperators.distance(local).between(first, second).toOption.get mustBe (math.sqrt(0.25 / 2) +- 1e-12)
    }

    "count a linked group once regardless of the number of matching leaves" in {
      val limits = RuleParameterBounds(
        numeric = Map(RuleTarget.AtrStop -> DoubleRange(1, 8, 0.25)),
        durations = Map(RuleTarget.TrendAge -> DurationRange(0.hours, 24.hours, 1.hour))
      )
      val first    = mirrored()
      val second   = mirrored(15.hours)
      val local    = TradeStrategySearchSpace.forStrategy(first, indicator, limits).toOption.get
      val distance = TradeStrategySearchOperators.distance(local)
      local.parameterCount mustBe 2
      distance.between(first, second).toOption.get mustBe (math.sqrt(0.25 / 2) +- 1e-12)
      distance.between(first, second) mustBe distance.between(second, first)
      distance.between(first, first) mustBe Right(0.0)
    }

    "measure ValueIs levels numerically and reject different categories" in {
      val root   = RulePath(RuleList.Open, 0)
      val first  = single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50))
      val second = single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 100))
      val local  = TradeStrategySearchSpace
        .forStrategy(
          first,
          indicator,
          RuleParameterBounds(
            numeric = Map(RuleTarget.At(RuleParameterPath(root, RuleParameter.Level), first) -> DoubleRange(0, 100, 1))
          )
        )
        .toOption
        .get
      val distance = TradeStrategySearchOperators.distance(local)
      local.parameterCount mustBe 1
      distance.between(first, second) mustBe Right(0.5)
      distance.between(first, single(C.ValueIs(ValueRole.Price, Rule.Operator.GreaterThan, 50))).isLeft mustBe true
      distance.between(first, single(C.ValueIs(ValueRole.Momentum, Rule.Operator.EqualTo, 50))).isLeft mustBe true
      TradeStrategySearchOperators.distance(space).between(template, changed(1, C.TrendIs(Direction.Downward))).isLeft mustBe true
    }

    "exclude singleton and omitted coordinates and reject inconsistent linked values" in {
      val first = mirrored()
      val local = TradeStrategySearchSpace
        .forStrategy(
          first,
          indicator,
          linkedBounds.copy(
            numeric = Map(RuleTarget.AtrStop -> DoubleRange(4, 4, 0.25)),
            durations = Map(RuleTarget.TrendAge -> DurationRange(0.hours, 24.hours, 1.hour))
          )
        )
        .toOption
        .get
      local.parameterCount mustBe 1
      TradeStrategySearchOperators.distance(local).between(first, mirrored(15.hours)) mustBe Right(0.5)
      val mixed = first.copy(openRules = first.openRules.updated(0, mirrored(15.hours).openRules.head))
      TradeStrategySearchOperators.distance(local).between(first, mixed).isLeft mustBe true
      val fixed = TradeStrategySearchSpace.forStrategy(first, indicator, RuleParameterBounds()).toOption.get
      TradeStrategySearchOperators.distance(fixed).between(first, first) mustBe Right(0.0)
    }
  }
}
