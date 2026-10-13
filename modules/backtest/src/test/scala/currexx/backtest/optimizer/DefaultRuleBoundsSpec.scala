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
  ): TestStrategy = TestStrategy(
    indicator,
    TradeStrategy(
      List(Rule(TradeAction.OpenLong, C.AllOf(List(C.TrendActiveFor(trend), C.PositionOpenFor(holding))))),
      List(Rule(TradeAction.ClosePosition, C.PriceMovedAgainstEntry(atr)))
    )
  )

  private def single(condition: C): TestStrategy =
    TestStrategy(indicator, TradeStrategy(List(Rule(TradeAction.OpenLong, condition)), Nil))

  private def round(
      target: OptimisationTarget = OptimisationTarget.Strategy,
      strategy: TestStrategy = allKinds()
  ): OptimisationRound =
    OptimisationRound("default-bounds", strategy, OptimisationRounds.gaParameters, ScoringFunction.Robust(), target = target)

  private def space(round: OptimisationRound): OptimisationSearchSpace =
    OptimisationSearchSpace.forRound(round).fold(error => fail(error.getMessage), identity)

  "Default rule bounds" should {
    "enable the documented numeric and duration ranges by default" in {
      val expected = RuleParameterBounds(
        numeric = Map(RuleTarget.AtrStop -> DoubleRange(0.5, 20.0, 0.5)),
        durations = Map(
          RuleTarget.TrendAge    -> DurationRange(0.hours, 2.days, 1.hour),
          RuleTarget.HoldingTime -> DurationRange(0.hours, 2.days, 1.hour)
        )
      )
      RuleParameterBounds.default mustBe expected
      List(OptimisationTarget.Strategy, OptimisationTarget.Both).foreach { target =>
        val configured = round(target)
        configured.ruleBounds mustBe expected
        val search = space(configured)
        search.strategyParameterCount mustBe 3
        val changed = allKinds(atr = 9.0, trend = 36.hours, holding = 0.hours)
        search.canonicalise(changed) mustBe Right(changed)
      }
    }

    "replace defaults completely with explicit bounds and keep omitted fields fixed" in {
      val overrideBounds = RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> DoubleRange(3.0, 5.0, 0.5)))
      val search         = space(round().copy(ruleBounds = overrideBounds))
      search.strategyParameterCount mustBe 1
      search.canonicalise(allKinds(atr = 5.0)) mustBe Right(allKinds(atr = 5.0))
      search.canonicalise(allKinds(trend = 6.hours, holding = 24.hours)).isLeft mustBe true
      search.resolveSeeds(List(allKinds(atr = 5.0, trend = 6.hours, holding = 24.hours))).map(_.head.effective) mustBe
        Right(Some(allKinds(atr = 5.0)))
    }

    "require manual ranges to search value levels or velocity thresholds" in {
      val cases = List(
        (C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50), RuleParameter.Level, DoubleRange(0, 100, 1)),
        (C.VelocityIsBelow(0.2), RuleParameter.Threshold, DoubleRange(0, 1, 0.1)),
        (C.VelocityCrossedLevel(0.1, Direction.Upward), RuleParameter.Level, DoubleRange(-1, 1, 0.1))
      )
      cases.foreach { (condition, parameter, range) =>
        val strategy  = single(condition)
        val defaulted = round(OptimisationTarget.Both, strategy)
        space(defaulted).strategyParameterCount mustBe 0
        val path     = RuleParameterPath(RulePath(RuleList.Open, 0), parameter)
        val explicit = defaulted.copy(ruleBounds = RuleParameterBounds(numeric = Map(RuleTarget.At(path, strategy.rules) -> range)))
        space(explicit).strategyParameterCount mustBe 1
      }
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
        val search = space(round(OptimisationTarget.Indicator, strategy).copy(ruleBounds = bounds))
        search.strategySpace mustBe None
        search.strategyParameterCount mustBe 0
        search.indicatorParameterCount mustBe baseline.indicatorParameterCount
        search.fixedIndicators mustBe baseline.fixedIndicators
        search.canonicalise(strategy) mustBe baseline.canonicalise(strategy)
      }
    }
  }
}
