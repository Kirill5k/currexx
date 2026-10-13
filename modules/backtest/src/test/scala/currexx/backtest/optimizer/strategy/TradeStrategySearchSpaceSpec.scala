package currexx.backtest.optimizer.strategy

import currexx.core.market.MomentumZone
import currexx.core.trade.{Rule, TradeAction}
import currexx.domain.market.TradeOrder
import currexx.domain.signal.{Direction, Indicator, ValueRole, ValueSource, ValueTransformation as VT, VolatilityRegime}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.*

class TradeStrategySearchSpaceSpec extends AnyWordSpec with Matchers {
  import Rule.Condition as C
  import RuleParameterBounds.{DoubleRange, DurationRange}
  import TradeStrategyFixtures.*

  private def compile(base: currexx.core.trade.TradeStrategy, limits: RuleParameterBounds) =
    TradeStrategySearchSpace.forStrategy(base, indicator, limits)

  "TradeStrategySearchSpace" should {
    "search only numeric and duration fields" in {
      space.parameterCount mustBe 6
      space.validate(template) mustBe Right(template)
    }

    "distinguish compatible rule structure from invalid numeric parameters" in {
      val offGrid = changed(19, C.PriceMovedAgainstEntry(4.13))
      space.validateStructure(offGrid) mustBe Right(())
      space.validate(offGrid).isLeft mustBe true
      space.validateStructure(changed(18, C.ValueIs(ValueRole.Price, Rule.Operator.GreaterThan, 50))).isLeft mustBe true
    }

    "keep actions, tree structure, ordering and every categorical field fixed" in {
      val categorical = List(
        0  -> C.TrendChangedTo(Direction.Downward),
        1  -> C.TrendIs(Direction.Downward),
        3  -> C.CrossoverOccurred(Direction.Downward),
        4  -> C.MomentumEntered(MomentumZone.Overbought),
        5  -> C.MomentumIsIn(MomentumZone.Neutral),
        6  -> C.MomentumIs(Direction.Still),
        7  -> C.PreviousVolatilityIs(VolatilityRegime.Low),
        8  -> C.VolatilityIs(VolatilityRegime.High),
        9  -> C.VelocityIs(Direction.Still),
        10 -> C.VelocityCrossedLevel(0.1, Direction.Downward),
        12 -> C.PositionIs(TradeOrder.Position.Sell),
        15 -> C.UpperBandCrossed(Direction.Downward),
        16 -> C.LowerBandCrossed(Direction.Upward),
        17 -> C.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward),
        17 -> C.PriceCrossedLine(ValueRole.Price, Direction.Downward),
        18 -> C.ValueIs(ValueRole.Price, Rule.Operator.GreaterThan, 50),
        18 -> C.ValueIs(ValueRole.Momentum, Rule.Operator.EqualTo, 50)
      )
      categorical.foreach { (index, condition) =>
        val candidate = changed(index, condition)
        space.validate(candidate).swap.toOption.get.getMessage must include(path(index).toString)
        space.projectSeed(candidate).isLeft mustBe true
      }
      val invalid = List(
        template.copy(openRules = template.openRules.map(_.copy(action = TradeAction.OpenShort))),
        template.copy(closeRules = template.openRules),
        single(C.AnyOf(conditions)),
        single(C.AllOf(conditions.reverse)),
        single(C.AllOf(conditions.dropRight(1))),
        single(C.Not(C.AllOf(conditions)))
      )
      invalid.foreach(candidate => space.validate(candidate).isLeft mustBe true)
    }

    "compare every fixed field after normalising numeric values inside Not" in {
      val leafPath = RulePath(RuleList.Open, 0, Vector(0))
      val base     = single(C.Not(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50)))
      val local    = compile(
        base,
        RuleParameterBounds(numeric = Map(RuleTarget.At(RuleParameterPath(leafPath, RuleParameter.Level), base) -> DoubleRange(0, 100, 1)))
      ).toOption.get
      val numericChange = single(C.Not(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 75)))
      local.validate(numericChange) mustBe Right(numericChange)
      List(
        single(C.Not(C.ValueIs(ValueRole.Price, Rule.Operator.GreaterThan, 75))),
        single(C.Not(C.ValueIs(ValueRole.Momentum, Rule.Operator.EqualTo, 75)))
      ).foreach { candidate =>
        local.validate(candidate).swap.toOption.get.getMessage must include(leafPath.toString)
        local.projectSeed(candidate).swap.toOption.get.getMessage must include(leafPath.toString)
      }
    }

    "report the first fixed-tree difference before a later invalid numeric leaf" in {
      val candidate = single(C.AllOf(conditions.updated(0, C.TrendChangedTo(Direction.Downward)).updated(10, C.NoPosition)))
      List(space.validate(candidate), space.projectSeed(candidate)).foreach { result =>
        val message = result.swap.toOption.get.getMessage
        message must include(path(0).toString)
        (message must not).include(path(10).toString)
      }
    }

    "validate inputs and direction domains even when conditions are fixed inside Not" in {
      val noInputs = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1))
      List(
        C.TrendIs(Direction.Upward),
        C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50),
        C.PriceCrossedLine(ValueRole.Price, Direction.Upward)
      ).foreach { leaf =>
        val error = TradeStrategySearchSpace.forStrategy(single(C.Not(leaf)), noInputs, RuleParameterBounds()).swap.toOption.get
        error.getMessage must include(RulePath(RuleList.Open, 0, Vector(0)).toString)
      }
      compile(single(C.TrendIs(Direction.Still)), RuleParameterBounds()).isLeft mustBe true
      List(C.MomentumIs(Direction.Still), C.VelocityIs(Direction.Still)).foreach { leaf =>
        compile(single(leaf), RuleParameterBounds()).isRight mustBe true
      }
      compile(single(C.NoPosition), RuleParameterBounds()).toOption.get.parameterCount mustBe 0
    }

    "link every matching kind across long, short, open, close and nested conditions" in {
      val base  = mirrored()
      val local = compile(base, linkedBounds).toOption.get
      local.parameterCount mustBe 3
      local.validate(mirrored(5.hours, 24.hours, 5.5)).isRight mustBe true
      val oneChanged = base.copy(openRules =
        base.openRules
          .updated(0, base.openRules.head.copy(conditions = C.AllOf(List(C.TrendIs(Direction.Upward), C.TrendActiveFor(5.hours)))))
      )
      local.validate(oneChanged).isLeft mustBe true
      local.projectSeed(oneChanged).isLeft mustBe true
      local.projectSeed(mirrored(5.hours, 24.hours, 5.5)).isRight mustBe true
    }

    "keep kind selectors linked after a condition is inserted and allow explicit single paths" in {
      val base     = mirrored()
      val inserted = base.copy(openRules =
        base.openRules.updated(
          0,
          base.openRules.head.copy(conditions = C.AllOf(List(C.NoPosition, C.TrendIs(Direction.Upward), C.TrendActiveFor(3.hours))))
        )
      )
      val local =
        compile(inserted, RuleParameterBounds(durations = Map(RuleTarget.TrendAge -> DurationRange(1.hour, 8.hours, 1.hour)))).toOption.get
      local.parameterCount mustBe 1
      val first       = RuleParameterPath(RulePath(RuleList.Open, 0, Vector(2)), RuleParameter.Duration)
      val independent =
        compile(
          inserted,
          RuleParameterBounds(durations = Map(RuleTarget.At(first, inserted) -> DurationRange(1.hour, 8.hours, 1.hour)))
        ).toOption.get
      val changed = inserted.copy(openRules =
        inserted.openRules.updated(
          0,
          inserted.openRules.head.copy(conditions = C.AllOf(List(C.NoPosition, C.TrendIs(Direction.Upward), C.TrendActiveFor(5.hours))))
        )
      )
      independent.parameterCount mustBe 1
      independent.validate(changed) mustBe Right(changed)
      local.validate(changed).isLeft mustBe true
      val stale = RuleTarget.At(first.copy(rule = first.rule.copy(conditions = Vector(1))), inserted)
      compile(inserted, RuleParameterBounds(durations = Map(stale -> DurationRange(1.hour, 8.hours, 1.hour)))).isLeft mustBe true
    }

    "ignore unused named targets but reject unmatched paths, overlap and wrong types" in {
      val duration = DurationRange(0.hours, 24.hours, 1.hour)
      val number   = DoubleRange(1, 8, 0.25)
      compile(single(C.NoPosition), RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> number))).toOption.get.parameterCount mustBe 0
      val missing = RuleTarget.At(RuleParameterPath(RulePath(RuleList.Open, 0), RuleParameter.AtrMultiplier), single(C.NoPosition))
      compile(single(C.NoPosition), RuleParameterBounds(numeric = Map(missing -> number))).isLeft mustBe true
      compile(mirrored(), RuleParameterBounds(numeric = Map(RuleTarget.TrendAge -> number))).isLeft mustBe true
      compile(mirrored(), RuleParameterBounds(durations = Map(RuleTarget.AtrStop -> duration))).isLeft mustBe true
      val path = RuleParameterPath(RulePath(RuleList.Open, 0, Vector(1)), RuleParameter.Duration)
      compile(
        mirrored(),
        RuleParameterBounds(durations = Map(RuleTarget.TrendAge -> duration, RuleTarget.At(path, mirrored()) -> duration))
      ).isLeft mustBe true
      val mismatched = mirrored().copy(openRules = mirrored(5.hours).openRules.updated(0, mirrored().openRules.head))
      compile(mismatched, linkedBounds).toOption.get.parameterCount mustBe 4
      val badField = RuleTarget.At(RuleParameterPath(RulePath(RuleList.Open, 0, Vector(0)), RuleParameter.Direction), mirrored())
      compile(mirrored(), RuleParameterBounds(numeric = Map(badField -> number))).isLeft mustBe true
      compile(template, RuleParameterBounds(numeric = Map(at(2, RuleParameter.Duration) -> number))).isLeft mustBe true
      compile(template, RuleParameterBounds(durations = Map(at(18, RuleParameter.Level) -> duration))).isLeft mustBe true
    }

    "freeze separate linked groups for distinct base values without changing their initial values" in {
      val base  = single(C.AllOf(List(C.TrendActiveFor(3.hours), C.TrendActiveFor(5.hours), C.Not(C.TrendActiveFor(3.hours)))))
      val local =
        compile(base, RuleParameterBounds(durations = Map(RuleTarget.TrendAge -> DurationRange(0.hours, 24.hours, 1.hour)))).toOption.get
      val changed   = single(C.AllOf(List(C.TrendActiveFor(7.hours), C.TrendActiveFor(9.hours), C.Not(C.TrendActiveFor(7.hours)))))
      val divergent = single(C.AllOf(List(C.TrendActiveFor(7.hours), C.TrendActiveFor(9.hours), C.Not(C.TrendActiveFor(8.hours)))))
      local.parameterCount mustBe 2
      local.validate(base) mustBe Right(base)
      local.projectSeed(base) mustBe Right(base)
      local.validate(changed) mustBe Right(changed)
      local.projectSeed(changed) mustBe Right(changed)
      local.validate(divergent).isLeft mustBe true
      local.projectSeed(divergent).isLeft mustBe true
    }

    "reject stale positional bounds after same-kind reordering, insertion or action changes" in {
      val original  = single(C.AllOf(List(C.TrendActiveFor(3.hours), C.TrendActiveFor(5.hours))))
      val selected  = RuleParameterPath(RulePath(RuleList.Open, 0, Vector(0)), RuleParameter.Duration)
      val range     = DurationRange(0.hours, 24.hours, 1.hour)
      val limits    = RuleParameterBounds(durations = Map(RuleTarget.At(selected, original) -> range))
      val reordered = single(C.AllOf(List(C.TrendActiveFor(5.hours), C.TrendActiveFor(3.hours))))
      val edited    = List(
        reordered,
        single(C.AllOf(List(C.TrendActiveFor(3.hours), C.TrendActiveFor(3.hours), C.TrendActiveFor(5.hours)))),
        original.copy(openRules = original.openRules.map(_.copy(action = TradeAction.OpenShort)))
      )
      compile(original.copy(), limits).isRight mustBe true
      edited.foreach { candidate =>
        val error = compile(candidate, limits).swap.toOption.get.getMessage
        error must include("Stale rule target")
        error must include(selected.toString)
      }
      compile(reordered, RuleParameterBounds(durations = Map(RuleTarget.At(selected, reordered) -> range))).isRight mustBe true
    }

    "reject a reordered positional value bound when only categorical meaning differs" in {
      val leaves =
        List(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50), C.ValueIs(ValueRole.Price, Rule.Operator.GreaterThan, 50))
      val original = single(C.AllOf(leaves))
      val selected = RuleParameterPath(RulePath(RuleList.Open, 0, Vector(0)), RuleParameter.Level)
      val limits   = RuleParameterBounds(numeric = Map(RuleTarget.At(selected, original) -> DoubleRange(0, 100, 1)))
      compile(original, limits).isRight mustBe true
      compile(single(C.AllOf(leaves.reverse)), limits).swap.toOption.get.getMessage must include("Stale rule target")
    }

    "restore omitted and singleton numeric fields without repairing active or categorical fields" in {
      val fixed   = compile(template, RuleParameterBounds()).toOption.get
      val altered = single(
        C.AllOf(
          conditions
            .updated(2, C.TrendActiveFor(1.hour))
            .updated(18, C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 1.25))
            .updated(19, C.PriceMovedAgainstEntry(5))
        )
      )
      fixed.parameterCount mustBe 0
      fixed.validate(altered).isLeft mustBe true
      fixed.projectSeed(altered) mustBe Right(template)
      val partlyFixed =
        compile(template, bounds.copy(numeric = bounds.numeric.updated(RuleTarget.AtrStop, DoubleRange(4, 4, 0.25)))).toOption.get
      partlyFixed.parameterCount mustBe 5
      partlyFixed.projectSeed(changed(19, C.PriceMovedAgainstEntry(99))) mustBe Right(template)
      partlyFixed.projectSeed(changed(10, C.VelocityCrossedLevel(99, Direction.Upward))).isLeft mustBe true
      partlyFixed.projectSeed(changed(1, C.TrendIs(Direction.Downward))).isLeft mustBe true
      compile(template, bounds.copy(numeric = bounds.numeric.updated(RuleTarget.AtrStop, DoubleRange(5, 5, 0.25)))).isLeft mustBe true
    }

    "restore a fixed linked group before validating seed equality" in {
      val local        = compile(mirrored(), linkedBounds.copy(numeric = Map(RuleTarget.AtrStop -> DoubleRange(4, 4, 0.25)))).toOption.get
      val changedStops = mirrored().copy(closeRules =
        List(
          Rule(
            TradeAction.ClosePosition,
            C.AnyOf(
              List(
                C.AllOf(List(C.PositionIs(TradeOrder.Position.Buy), C.PositionOpenFor(12.hours), C.PriceMovedAgainstEntry(6))),
                C.AllOf(List(C.PositionIs(TradeOrder.Position.Sell), C.PositionOpenFor(12.hours), C.PriceMovedAgainstEntry(7)))
              )
            )
          )
        )
      )
      local.projectSeed(changedStops) mustBe Right(mirrored())
      local.parameterCount mustBe 2
    }

    "require finite ordered bounds, positive steps and exact grids" in {
      List(
        DoubleRange(Double.NaN, 8, 0.25),
        DoubleRange(1, Double.PositiveInfinity, 1),
        DoubleRange(8, 1, 0.25),
        DoubleRange(1, 8, 0),
        DoubleRange(0, 8, 0.25),
        DoubleRange(1, 8, 0.3)
      ).foreach { range =>
        compile(template, bounds.copy(numeric = bounds.numeric.updated(RuleTarget.AtrStop, range))).isLeft mustBe true
      }
      List(
        DurationRange((-1).hour, 4.hours, 1.hour),
        DurationRange(0.hours, 4.hours, 0.hours),
        DurationRange(4.hours, 1.hour, 1.hour),
        DurationRange(0.hours, 4.hours, 90.minutes)
      ).foreach { range =>
        compile(template, bounds.copy(durations = bounds.durations.updated(RuleTarget.TrendAge, range))).isLeft mustBe true
      }
      space.validate(changed(19, C.PriceMovedAgainstEntry(4.13))).isLeft mustBe true
      space.validate(changed(2, C.TrendActiveFor(90.minutes))).isLeft mustBe true
      compile(changed(19, C.PriceMovedAgainstEntry(4.13)), bounds).isLeft mustBe true
    }

    "validate fixed numeric values and retain the required ATR inputs" in {
      List(
        C.VelocityIsBelow(-0.1),
        C.VelocityCrossedLevel(Double.NaN, Direction.Upward),
        C.PriceMovedAgainstEntry(0),
        C.PositionOpenFor((-1).hour),
        C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, Double.PositiveInfinity)
      ).foreach { leaf =>
        compile(single(leaf), RuleParameterBounds()).isLeft mustBe true
      }
      val wrong = Indicator.compositeAnyOf(
        Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(2)),
        Indicator.ValueTracking(ValueRole.Volatility, ValueSource.Close, VT.ATR(14))
      )
      TradeStrategySearchSpace
        .forStrategy(
          single(C.PriceMovedAgainstEntry(4)),
          wrong,
          RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> DoubleRange(1, 8, 0.25)))
        )
        .isLeft mustBe true
    }
  }
}
