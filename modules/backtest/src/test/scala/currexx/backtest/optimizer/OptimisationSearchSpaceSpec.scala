package currexx.backtest.optimizer

import currexx.backtest.TestStrategy
import currexx.backtest.optimizer.OptimisationSearchSpace.SeedDisposition
import currexx.backtest.optimizer.strategy.{RuleList, RuleParameter, RuleParameterBounds, RuleParameterPath, RulePath, RuleTarget}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.*

class OptimisationSearchSpaceSpec extends AnyWordSpec with Matchers {
  private val base = TestStrategy(
    Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(20)),
    TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.TrendActiveFor(3.hours))), Nil)
  )
  private val changedRules = base.rules.copy(openRules = List(Rule(TradeAction.OpenLong, Rule.Condition.TrendActiveFor(9.hours))))
  private val changed      = base.copy(indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(30)), rules = changedRules)
  private val ageBounds    =
    RuleParameterBounds(durations = Map(RuleTarget.TrendAge -> RuleParameterBounds.DurationRange(0.hours, 24.hours, 1.hour)))
  private def round(
      target: OptimisationTarget,
      strategy: TestStrategy = base,
      bounds: RuleParameterBounds = RuleParameterBounds()
  ): OptimisationRound =
    OptimisationRound(
      "search-space",
      strategy,
      OptimisationRounds.gaParameters,
      ScoringFunction.Robust(),
      target = target,
      ruleBounds = bounds
    )
  private def space(round: OptimisationRound): OptimisationSearchSpace =
    OptimisationSearchSpace.forRound(round).fold(error => fail(error.getMessage), identity)

  "OptimisationSearchSpace" should {
    "restore inactive components before resolving complete seed identities" in {
      val indicator = space(round(OptimisationTarget.Indicator))
      val strategy  = space(round(OptimisationTarget.Strategy, bounds = ageBounds))
      val both      = space(round(OptimisationTarget.Both, bounds = ageBounds))
      indicator.canonicalise(changed) mustBe Right(changed.copy(rules = base.rules))
      strategy.canonicalise(changed) mustBe Right(changed.copy(indicator = base.indicator))
      both.canonicalise(changed) mustBe Right(changed)
      val seeds = List(base.copy(rules = changedRules), changed, changed.copy(rules = base.rules))
      indicator.resolveSeeds(seeds).map(_.map(_.disposition)) mustBe Right(
        List(SeedDisposition.TargetDuplicate, SeedDisposition.Accepted, SeedDisposition.SeedDuplicate(1))
      )
      strategy.resolveSeeds(seeds).map(_.map(_.disposition)) mustBe Right(
        List(SeedDisposition.Accepted, SeedDisposition.SeedDuplicate(0), SeedDisposition.TargetDuplicate)
      )
      both.resolveSeeds(seeds).map(_.map(_.disposition)) mustBe Right(List.fill(3)(SeedDisposition.Accepted))
      List(indicator, strategy, both)
        .map(_.target) mustBe List(OptimisationTarget.Indicator, OptimisationTarget.Strategy, OptimisationTarget.Both)
      indicator.parameterCount mustBe indicator.indicatorParameterCount
      strategy.parameterCount mustBe strategy.strategyParameterCount
      both.parameterCount mustBe indicator.parameterCount + strategy.parameterCount
    }

    "validate active schemas and report an incompatible seed's reason" in {
      val otherShape = TestStrategy(Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1)), TradeStrategy(Nil, Nil))
      val indicator  = space(round(OptimisationTarget.Indicator))
      val strategy   = space(round(OptimisationTarget.Strategy, bounds = ageBounds))
      val both       = space(round(OptimisationTarget.Both, bounds = ageBounds))
      indicator.canonicalise(base.copy(rules = otherShape.rules)) mustBe Right(base)
      strategy.canonicalise(base.copy(indicator = otherShape.indicator)) mustBe Right(base)
      both.canonicalise(base.copy(rules = otherShape.rules)).isLeft mustBe true
      val rejected = both.resolveSeeds(List(otherShape)).toOption.get.head
      rejected.disposition mustBe SeedDisposition.Incompatible
      rejected.effective mustBe None
      rejected.reason.exists(_.nonEmpty) mustBe true
    }

    "retain fixed-role indicator dependencies in joint numeric searches" in {
      val momentum = Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(8))
      val strength = Indicator.ValueTracking(ValueRole.TrendStrength, ValueSource.Close, VT.ADX(14))
      val price    = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1))
      val template = TestStrategy(
        Indicator.compositeAnyOf(momentum, strength, price),
        TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 40))), Nil)
      )
      val path   = RuleParameterPath(RulePath(RuleList.Open, 0), RuleParameter.Level)
      val bounds = RuleParameterBounds(numeric = Map(RuleTarget.At(path, template.rules) -> RuleParameterBounds.DoubleRange(0, 100, 1)))
      val both   = space(round(OptimisationTarget.Both, template, bounds))
      both.fixedIndicators must contain(strength)
      both.fixedIndicators must contain(price)
      both.fixedIndicators must not contain momentum
      val seed = template.copy(rules =
        TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.ValueIs(ValueRole.TrendStrength, Rule.Operator.GreaterThan, 40))), Nil)
      )
      val restored = both.resolveSeeds(List(seed)).toOption.get.head
      restored.disposition mustBe SeedDisposition.TargetDuplicate
      restored.effective mustBe Some(template)
      restored.usedBaseRules mustBe true
      restored.reason.exists(_.contains(RulePath(RuleList.Open, 0).toString)) mustBe true
      space(round(OptimisationTarget.Indicator, template)).resolveSeeds(List(seed)).map(_.head.disposition) mustBe Right(
        SeedDisposition.TargetDuplicate
      )
    }

    "retain only the indicators of structurally different Both seeds before deduplication" in {
      val changedTree  = changed.copy(rules = TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.NoPosition))))
      val invalidLevel =
        changed.copy(rules = TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.TrendActiveFor(90.minutes))), Nil))
      val both     = space(round(OptimisationTarget.Both, bounds = ageBounds))
      val resolved = both.resolveSeeds(List(changedTree, changed.copy(rules = base.rules), invalidLevel)).toOption.get
      resolved.map(_.disposition) mustBe List(SeedDisposition.Accepted, SeedDisposition.SeedDuplicate(0), SeedDisposition.Incompatible)
      resolved.head.effective mustBe Some(changed.copy(rules = base.rules))
      resolved.head.usedBaseRules mustBe true
      resolved.head.reason.get must include("Using seed indicators with base rules")
      resolved.last.usedBaseRules mustBe false
      resolved.last.reason.get must include("outside the domain")
      both.canonicalise(changedTree).isLeft mustBe true
      val strategy = space(round(OptimisationTarget.Strategy, bounds = ageBounds))
      strategy.resolveSeeds(List(changedTree)).toOption.get.head.disposition mustBe SeedDisposition.Incompatible
    }

    "reject Strategy rounds without an active rule coordinate and let Both search the indicators" in {
      val singleton =
        RuleParameterBounds(durations = Map(RuleTarget.TrendAge -> RuleParameterBounds.DurationRange(3.hours, 3.hours, 1.hour)))
      List(RuleParameterBounds(), singleton).foreach { bounds =>
        OptimisationSearchSpace.forRound(round(OptimisationTarget.Strategy, bounds = bounds)).swap.toOption.get.getMessage must include(
          "at least one active rule parameter"
        )
        val both = space(round(OptimisationTarget.Both, bounds = bounds))
        both.target mustBe OptimisationTarget.Both
        both.strategyParameterCount mustBe 0
        both.indicatorParameterCount must be > 0
        both.canonicalise(base.copy(rules = changedRules)).isLeft mustBe true
        both.projectSeeds(List(changed)) mustBe Right(List(changed.copy(rules = base.rules)))
      }
      OptimisationSearchSpace.forRound(round(OptimisationTarget.Indicator)).isRight mustBe true
    }

    "ignore inactive rule bounds and reject unknown fixed indicator pins in every mode" in {
      space(round(OptimisationTarget.Indicator, bounds = ageBounds)).strategyParameterCount mustBe 0
      val unknown = Indicator.TrendChangeDetection(ValueSource.Open, VT.EMA(10))
      OptimisationTarget.values.foreach { target =>
        val bounds = if (target == OptimisationTarget.Indicator) RuleParameterBounds() else ageBounds
        OptimisationSearchSpace.forRound(round(target, bounds = bounds).copy(fixedIndicators = Set(unknown))).isLeft mustBe true
      }
    }

    "reject fixed indicator settings in Strategy rounds because all indicators are fixed" in {
      val strategyRound = round(OptimisationTarget.Strategy, bounds = ageBounds).copy(fixedIndicators = Set(base.indicator))
      OptimisationSearchSpace.forRound(strategyRound).swap.toOption.get.getMessage mustBe
        "fixedIndicators is not supported for Strategy rounds; all indicators are already fixed"
      List(OptimisationTarget.Indicator, OptimisationTarget.Both).foreach { target =>
        OptimisationSearchSpace.forRound(strategyRound.copy(target = target)).isRight mustBe true
      }
      space(strategyRound.copy(fixedIndicators = Set.empty)).fixedIndicators mustBe Set(base.indicator)
    }

    "restore inactive numeric fields before checking active seed parameters" in {
      val rules = TradeStrategy(
        List(
          Rule(
            TradeAction.OpenLong,
            Rule.Condition.AllOf(
              List(
                Rule.Condition.TrendActiveFor(3.hours),
                Rule.Condition.PositionOpenFor(2.hours)
              )
            )
          )
        ),
        Nil
      )
      val template = base.copy(rules = rules)
      val search   = space(round(OptimisationTarget.Strategy, template, ageBounds))
      val seed     = template.copy(rules =
        TradeStrategy(
          List(
            Rule(
              TradeAction.OpenLong,
              Rule.Condition.AllOf(
                List(
                  Rule.Condition.TrendActiveFor(9.hours),
                  Rule.Condition.PositionOpenFor(8.hours)
                )
              )
            )
          ),
          Nil
        )
      )
      val effective = template.copy(rules =
        TradeStrategy(
          List(
            Rule(
              TradeAction.OpenLong,
              Rule.Condition.AllOf(
                List(
                  Rule.Condition.TrendActiveFor(9.hours),
                  Rule.Condition.PositionOpenFor(2.hours)
                )
              )
            )
          ),
          Nil
        )
      )
      search.canonicalise(seed).isLeft mustBe true
      search.resolveSeeds(List(seed)).map(_.head.effective) mustBe Right(Some(effective))
      val offGrid  = base.copy(rules = TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.TrendActiveFor(90.minutes))), Nil))
      val rejected = space(round(OptimisationTarget.Strategy, bounds = ageBounds)).resolveSeeds(List(offGrid)).toOption.get.head
      rejected.disposition mustBe SeedDisposition.Incompatible
      rejected.reason.exists(_.nonEmpty) mustBe true
    }
  }
}
