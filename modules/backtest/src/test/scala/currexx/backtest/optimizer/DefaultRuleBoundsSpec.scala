package currexx.backtest.optimizer

import currexx.backtest.TestStrategy
import currexx.backtest.optimizer.strategy.{RuleList, RuleParameter, RuleParameterBounds, RuleParameterPath, RulePath, RuleTarget}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Direction, Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.*

class DefaultRuleBoundsSpec extends AnyWordSpec with Matchers {
  import Rule.Condition as C
  import RuleParameterBounds.{DoubleRange, DurationRange}

  private val indicator = Indicator.compositeAnyOf(
    Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(20)),
    Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1)),
    Indicator.ValueTracking(ValueRole.Volatility, ValueSource.Close, VT.ATR(14)),
    Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(8)),
    Indicator.ValueTracking(ValueRole.Velocity, ValueSource.Close, VT.KalmanVelocity(0.5, 0.1))
  )

  private def allKinds(
      atr: Double = 4.0,
      trend: FiniteDuration = 3.hours,
      holding: FiniteDuration = 12.hours
  ): TestStrategy = {
    val entries = List(C.TrendActiveFor(trend), C.PositionOpenFor(holding))
    TestStrategy(
      indicator,
      TradeStrategy(
        List(
          Rule(TradeAction.OpenLong, C.AllOf(entries)),
          Rule(TradeAction.OpenShort, C.Not(C.AnyOf(entries)))
        ),
        List(
          Rule(TradeAction.ClosePosition, C.Not(C.PriceMovedAgainstEntry(atr))),
          Rule(TradeAction.ClosePosition, C.PriceMovedAgainstEntry(atr))
        )
      )
    )
  }

  private def single(condition: C): TestStrategy =
    TestStrategy(indicator, TradeStrategy(List(Rule(TradeAction.OpenLong, condition)), Nil))

  private def round(
      target: OptimisationTarget = OptimisationTarget.Strategy,
      strategy: TestStrategy = allKinds(),
      bounds: RuleParameterBounds = RuleParameterBounds.default
  ): OptimisationRound =
    OptimisationRound(
      "default-bounds",
      strategy,
      OptimisationRounds.gaParameters,
      ScoringFunction.Robust(),
      target = target,
      ruleBounds = bounds
    )

  private def space(round: OptimisationRound): OptimisationSearchSpace =
    OptimisationSearchSpace.forRound(round).fold(error => fail(error.getMessage), identity)

  "Default rule bounds" should {
    "define the documented numeric and duration ranges" in {
      RuleParameterBounds.default mustBe RuleParameterBounds(
        numeric = Map(RuleTarget.AtrStop -> DoubleRange(0.5, 20.0, 0.5)),
        durations = Map(
          RuleTarget.TrendAge    -> DurationRange(0.hours, 2.days, 1.hour),
          RuleTarget.HoldingTime -> DurationRange(0.hours, 2.days, 1.hour)
        )
      )
    }

    "apply named defaults only to present kinds and link repeated nested occurrences once" in {
      val nested = single(C.AllOf(List(C.Not(C.TrendActiveFor(3.hours)), C.AnyOf(List(C.TrendActiveFor(3.hours), C.NoPosition)))))
      List(OptimisationTarget.Strategy, OptimisationTarget.Both).foreach { target =>
        val configured = round(target, nested)
        configured.ruleBounds mustBe RuleParameterBounds.default
        space(configured).strategyParameterCount mustBe 1
        space(round(target)).strategyParameterCount mustBe 3
      }
    }

    "replace defaults completely with explicit bounds and keep omitted fields fixed" in {
      val overrideBounds = RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> DoubleRange(3.0, 5.0, 0.5)))
      val configured     = round(bounds = overrideBounds)
      configured.ruleBounds mustBe overrideBounds
      val search = space(configured)
      search.strategyParameterCount mustBe 1
      search.canonicalise(allKinds(atr = 5.0)) mustBe Right(allKinds(atr = 5.0))
      search.canonicalise(allKinds(trend = 6.hours, holding = 24.hours)).isLeft mustBe true
      search.resolveSeeds(List(allKinds(atr = 5.0, trend = 6.hours, holding = 24.hours))).map(_.head.effective) mustBe
        Right(Some(allKinds(atr = 5.0)))

      val longAge      = RuleTarget.At(RuleParameterPath(RulePath(RuleList.Open, 0, Vector(0)), RuleParameter.Duration), allKinds().rules)
      val pathOverride = RuleParameterBounds(durations = Map(longAge -> DurationRange(0.hours, 12.hours, 1.hour)))
      val exactPath    = space(round(bounds = pathOverride))
      exactPath.strategyParameterCount mustBe 1
      val changedLong = allKinds().copy(rules =
        allKinds().rules.copy(openRules =
          allKinds().rules.openRules.updated(
            0,
            Rule(TradeAction.OpenLong, C.AllOf(List(C.TrendActiveFor(6.hours), C.PositionOpenFor(12.hours))))
          )
        )
      )
      exactPath.canonicalise(changedLong) mustBe Right(changedLong)
      exactPath.canonicalise(allKinds(trend = 6.hours)).isLeft mustBe true
    }

    "treat explicit empty or singleton bounds as an exact replacement with no active rule search" in {
      val singleton = RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> DoubleRange(4.0, 4.0, 0.25)))
      List(RuleParameterBounds(), singleton).foreach { bounds =>
        val configured = round(bounds = bounds)
        configured.ruleBounds mustBe bounds
        OptimisationSearchSpace.forRound(configured).swap.toOption.get.getMessage must include("at least one active rule parameter")
        val both = space(configured.copy(target = OptimisationTarget.Both))
        both.strategyParameterCount mustBe 0
        both.indicatorParameterCount must be > 0
      }
    }

    "require manual ranges to search value levels or velocity thresholds" in {
      val cases = List(
        (C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50), RuleParameter.Level, DoubleRange(0, 100, 1)),
        (C.VelocityIsBelow(0.2), RuleParameter.Threshold, DoubleRange(0, 1, 0.1)),
        (C.VelocityCrossedLevel(0.1, Direction.Upward), RuleParameter.Level, DoubleRange(-1, 1, 0.1))
      )
      cases.foreach { (condition, parameter, range) =>
        val strategy = single(condition)
        List(OptimisationTarget.Strategy, OptimisationTarget.Both).foreach { target =>
          val defaulted = round(target, strategy)
          target match
            case OptimisationTarget.Strategy =>
              OptimisationSearchSpace.forRound(defaulted).swap.toOption.get.getMessage must include("at least one active rule parameter")
            case _ => space(defaulted).strategyParameterCount mustBe 0
          val path     = RuleParameterPath(RulePath(RuleList.Open, 0), parameter)
          val explicit = defaulted.copy(ruleBounds = RuleParameterBounds(numeric = Map(RuleTarget.At(path, strategy.rules) -> range)))
          space(explicit).strategyParameterCount mustBe 1
        }
      }
    }

    "apply the same concrete bounds to the current tree after copying a round" in {
      val original    = round()
      val holdingOnly = original.copy(strategy = single(C.PositionOpenFor(12.hours)))
      holdingOnly.ruleBounds mustBe RuleParameterBounds.default
      space(holdingOnly).strategyParameterCount mustBe 1
      val indicatorOnly = original.copy(target = OptimisationTarget.Indicator)
      indicatorOnly.ruleBounds mustBe original.ruleBounds
      space(indicatorOnly).strategyParameterCount mustBe 0
      space(indicatorOnly.copy(target = OptimisationTarget.Both)).strategyParameterCount mustBe 3

      val overrideBounds  = RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> DoubleRange(2, 6, 0.5)))
      val explicit        = original.copy(ruleBounds = overrideBounds)
      val changedStrategy = explicit.copy(strategy = holdingOnly.strategy)
      changedStrategy.ruleBounds mustBe overrideBounds
      OptimisationSearchSpace.forRound(changedStrategy).swap.toOption.get.getMessage must include("at least one active rule parameter")
      val changedTarget = explicit.copy(target = OptimisationTarget.Indicator)
      changedTarget.ruleBounds mustBe overrideBounds
      space(changedTarget).strategyParameterCount mustBe 0
    }

    "ignore unmatched named targets while rejecting invalid exact paths and wrong target kinds" in {
      val strategy   = single(C.TrendActiveFor(3.hours))
      val configured = RuleParameterBounds(
        numeric = Map(RuleTarget.AtrStop -> DoubleRange(1, 8, 0.25)),
        durations = Map(
          RuleTarget.TrendAge    -> DurationRange(0.hours, 12.hours, 1.hour),
          RuleTarget.HoldingTime -> DurationRange(1.hour, 72.hours, 1.hour)
        )
      )
      space(round(strategy = strategy, bounds = configured)).strategyParameterCount mustBe 1
      val absent        = RuleTarget.At(RuleParameterPath(RulePath(RuleList.Open, 9), RuleParameter.Level), strategy.rules)
      val exactDuration = RuleTarget.At(RuleParameterPath(RulePath(RuleList.Open, 0), RuleParameter.Duration), strategy.rules)
      val invalid       = List(
        configured.copy(numeric = configured.numeric.updated(absent, DoubleRange(0, 100, 1))),
        configured.copy(numeric = configured.numeric.updated(exactDuration, DoubleRange(0, 100, 1))),
        configured.copy(numeric = configured.numeric.updated(RuleTarget.TrendAge, DoubleRange(0, 100, 1))),
        configured.copy(durations = configured.durations.updated(RuleTarget.AtrStop, DurationRange(1.hour, 24.hours, 1.hour)))
      )
      invalid.foreach(bounds => OptimisationSearchSpace.forRound(round(strategy = strategy, bounds = bounds)).isLeft mustBe true)
    }

    "cover longer durations and larger stops while keeping exact range and grid checks" in {
      val valid = List(
        allKinds(atr = 9.0),
        allKinds(trend = 25.hours),
        allKinds(trend = 36.hours),
        allKinds(holding = 0.hours),
        allKinds(holding = 36.hours)
      )
      valid.foreach { strategy =>
        val search = space(round(strategy = strategy))
        search.strategyParameterCount mustBe 3
        search.canonicalise(strategy) mustBe Right(strategy)
      }
      val invalid = List(
        allKinds(atr = 20.1),
        allKinds(atr = 4.1),
        allKinds(trend = 2.days + 1.hour),
        allKinds(trend = 90.minutes),
        allKinds(holding = 2.days + 1.hour),
        allKinds(holding = 90.minutes)
      )
      invalid.foreach { strategy =>
        val configured = round(strategy = strategy)
        configured.ruleBounds mustBe RuleParameterBounds.default
        OptimisationSearchSpace.forRound(configured).isLeft mustBe true
      }
      val explicit = RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> DoubleRange(1, 8, 0.25)))
      OptimisationSearchSpace.forRound(round(strategy = allKinds(atr = 9), bounds = explicit)).isLeft mustBe true
    }

    "preserve unequal mirrored values as separate linked groups" in {
      val base    = allKinds()
      val unequal = List(
        base.copy(rules =
          base.rules.copy(openRules =
            base.rules.openRules.updated(
              1,
              Rule(TradeAction.OpenShort, C.Not(C.AnyOf(List(C.TrendActiveFor(4.hours), C.PositionOpenFor(12.hours)))))
            )
          )
        ),
        base.copy(rules =
          base.rules.copy(openRules =
            base.rules.openRules.updated(
              1,
              Rule(TradeAction.OpenShort, C.Not(C.AnyOf(List(C.TrendActiveFor(3.hours), C.PositionOpenFor(13.hours)))))
            )
          )
        ),
        base.copy(rules =
          base.rules.copy(closeRules = base.rules.closeRules.updated(1, Rule(TradeAction.ClosePosition, C.PriceMovedAgainstEntry(5))))
        )
      )
      unequal.foreach { strategy =>
        val search = space(round(strategy = strategy))
        search.strategyParameterCount mustBe 4
        search.canonicalise(strategy) mustBe Right(strategy)
      }
      val deliberateOverride = RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> DoubleRange(1, 10, 0.25)))
      space(round(strategy = allKinds(atr = 9), bounds = deliberateOverride)).strategyParameterCount mustBe 1
    }

    "ignore every rule-bound configuration in indicator-only rounds" in {
      val strategy          = allKinds(atr = 9, trend = 90.minutes)
      val invalidRuleBounds = RuleParameterBounds(numeric =
        Map(
          RuleTarget.At(RuleParameterPath(RulePath(RuleList.Open, 9), RuleParameter.Duration), strategy.rules) -> DoubleRange(
            Double.NaN,
            1,
            -1
          )
        )
      )
      val baseline = space(round(OptimisationTarget.Indicator, strategy))
      List(RuleParameterBounds.default, RuleParameterBounds(), invalidRuleBounds).foreach { bounds =>
        val configured = round(OptimisationTarget.Indicator, strategy, bounds)
        configured.ruleBounds mustBe bounds
        val search = space(configured)
        search.strategySpace mustBe None
        search.strategyParameterCount mustBe 0
        search.indicatorParameterCount mustBe baseline.indicatorParameterCount
        search.fixedIndicators mustBe baseline.fixedIndicators
        search.canonicalise(strategy) mustBe baseline.canonicalise(strategy)
      }
    }
  }
}
