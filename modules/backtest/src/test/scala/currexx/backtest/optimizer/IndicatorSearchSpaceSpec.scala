package currexx.backtest.optimizer

import cats.data.NonEmptyList
import currexx.backtest.TestStrategy
import currexx.core.market.MomentumZone
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.market.TradeOrder
import currexx.domain.signal.{
  CombinationLogic,
  Direction,
  Indicator,
  MovingAverage,
  ValueRole,
  ValueSource,
  ValueTransformation as VT,
  VolatilityRegime
}
import kirill5k.common.cats.test.IOWordSpec

import scala.concurrent.duration.*

class IndicatorSearchSpaceSpec extends IOWordSpec {
  private val trend = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(20))
  private val raw   = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1))

  private def strategy(indicator: Indicator, conditions: List[Rule.Condition] = Nil): TestStrategy =
    TestStrategy(indicator, TradeStrategy(conditions.map(Rule(TradeAction.OpenLong, _)), Nil))

  private def tracker(role: ValueRole): Indicator = Indicator.ValueTracking(role, ValueSource.Close, VT.SMA(10))

  private def searchSpace(strategy: TestStrategy, fixedIndicators: Set[Indicator] = Set.empty): IndicatorSearchSpace =
    IndicatorSearchSpace
      .forStrategy(strategy, fixedIndicators)
      .fold(error => fail(s"Expected a valid search space: ${error.getMessage}"), identity)

  private def leaves(projected: Either[Throwable, Option[Indicator]]): List[Indicator] = projected match
    case Right(Some(Indicator.Composite(children, CombinationLogic.Any))) => children.toList
    case Right(None)                                                      => Nil
    case other                                                            => fail(s"Expected valid flat Any genes, got $other")

  private def replace(indicator: Indicator, index: Int, replacement: Indicator): Indicator = indicator match
    case Indicator.Composite(children, combinator) =>
      Indicator.Composite(NonEmptyList.fromListUnsafe(children.toList.updated(index, replacement)), combinator)
    case other => fail(s"Expected a composite, got $other")

  "IndicatorSearchSpace" should {
    "keep s10's raw close fixed while retaining its ATR and momentum search dimensions" in {
      val space   = searchSpace(TestStrategy.s10)
      val changed = replace(TestStrategy.s10.indicator, 4, Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(80)))

      space.fixedIndicators mustBe Set(raw)
      space.canonicalise(changed) mustBe Right(TestStrategy.s10.indicator)
      leaves(space.project(changed)) must have size 6
      leaves(space.project(changed)) must contain(Indicator.ValueTracking(ValueRole.Volatility, ValueSource.Close, VT.ATR(14)))
      space.project(changed).flatMap(space.restore) mustBe Right(TestStrategy.s10.indicator)
    }

    "exclude s5's unread tracker from genes without removing it from the strategy" in {
      val space   = searchSpace(TestStrategy.s5_optimized_v2)
      val changed =
        replace(TestStrategy.s5_optimized_v2.indicator, 4, Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(30)))

      space.fixedIndicators mustBe Set(Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(8)))
      space.project(changed) mustBe space.project(TestStrategy.s5_optimized_v2.indicator)
      leaves(space.project(changed)) must have size 4
      space.project(changed).flatMap(space.restore) mustBe Right(TestStrategy.s5_optimized_v2.indicator)
      space.canonicalise(changed).flatMap(space.canonicalise) mustBe space.canonicalise(changed)
    }

    "preserve nested All and Any topology, ordering and explicitly fixed subtrees" in {
      val frozen       = Indicator.compositeAllOf(trend, raw)
      val active       = Indicator.ThresholdCrossing(ValueSource.HLC3, VT.RSX(12), 70, 30)
      val base         = Indicator.compositeAllOf(frozen, Indicator.compositeAnyOf(active, trend))
      val space        = searchSpace(strategy(base), Set(frozen))
      val newThreshold = Indicator.ThresholdCrossing(ValueSource.HLC3, VT.RSX(18), 75, 25)
      val newTrend     = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(30))
      val newGenes     = Some(Indicator.compositeAnyOf(newThreshold, newTrend))
      val expected     = Indicator.compositeAllOf(frozen, Indicator.compositeAnyOf(newThreshold, newTrend))

      space.restore(newGenes) mustBe Right(expected)
      space.project(expected) mustBe Right(newGenes)
      space.project(base).flatMap(space.restore) mustBe Right(base)
    }

    "freeze every exact occurrence of an explicitly fixed value" in {
      val active    = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(30))
      val base      = Indicator.compositeAnyOf(trend, active, Indicator.compositeAllOf(trend))
      val space     = searchSpace(strategy(base), Set(trend))
      val changed   = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(50))
      val candidate = Indicator.compositeAnyOf(changed, active, Indicator.compositeAllOf(changed))

      space.fixedIndicators mustBe Set(trend)
      space.canonicalise(candidate) mustBe Right(base)
      space.project(candidate) mustBe Right(Some(Indicator.compositeAnyOf(active)))
      space.restore(Some(Indicator.compositeAnyOf(changed))) mustBe
        Right(Indicator.compositeAnyOf(trend, changed, Indicator.compositeAllOf(trend)))
    }

    "support a single active leaf without changing its phenotype into a composite" in {
      val space = searchSpace(strategy(trend))
      space.project(trend) mustBe Right(Some(Indicator.compositeAnyOf(trend)))
      space.project(trend).flatMap(space.restore) mustBe Right(trend)
    }

    "allow an entirely fixed strategy and skip all bounds repair" in {
      val base  = Indicator.compositeAnyOf(raw, trend)
      val space = searchSpace(strategy(base), Set(base))
      space.project(base) mustBe Right(None)
      space.restore(None) mustBe Right(base)
      space.canonicalise(replace(base, 1, Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(1000)))) mustBe Right(base)
      space.restore(Some(Indicator.compositeAnyOf(trend))).left.map(_.getMessage) mustBe
        Left("Search genes do not match the number of active indicator leaves")
    }

    "fix a raw close even when the price value is read directly" in {
      val space =
        searchSpace(strategy(raw, List(Rule.Condition.ValueIs(ValueRole.Price, Rule.Operator.GreaterThan, 1))))
      space.fixedIndicators mustBe Set(raw)
      space.project(raw) mustBe Right(None)
    }

    "leave a used smoothed price searchable rather than treating every price tracker as raw" in {
      val price = tracker(ValueRole.Price)
      val space = searchSpace(strategy(price, List(Rule.Condition.PriceMovedAgainstEntry(2))))
      space.fixedIndicators mustBe Set.empty
      space.project(price) mustBe Right(Some(Indicator.compositeAnyOf(price)))
    }

    "return unknown fixed values as a Left rather than throwing" in {
      val base    = strategy(Indicator.compositeAnyOf(trend, raw))
      val unknown = Set[Indicator](Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(30)), Indicator.compositeAllOf(trend))

      IndicatorSearchSpace.forStrategy(base, unknown + trend) match {
        case Left(error) =>
          error.getMessage must include("Unknown fixed indicators:")
          unknown.foreach(indicator => error.getMessage must include(indicator.toString))
        case Right(_) => fail("Expected unknown fixed indicators to fail validation")
      }
      succeed
    }

    "preserve matching explicit and automatic pins alongside searchable leaves" in {
      val active    = Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(12), 70, 30)
      val base      = Indicator.compositeAnyOf(trend, raw, active)
      val space     = searchSpace(strategy(base), Set(trend))
      val changed   = Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(18), 75, 25)
      val candidate = Indicator.compositeAnyOf(
        Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(60)),
        Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(12)),
        changed
      )

      space.fixedIndicators mustBe Set(trend, raw)
      space.project(base) mustBe Right(Some(Indicator.compositeAnyOf(active)))
      space.project(candidate) mustBe Right(Some(Indicator.compositeAnyOf(changed)))
      space.canonicalise(candidate) mustBe Right(Indicator.compositeAnyOf(trend, raw, changed))
      space.project(candidate).flatMap(space.restore) mustBe Right(Indicator.compositeAnyOf(trend, raw, changed))
    }

    "reject a missing chromosome, incorrect count and nested or wrongly shaped genes" in {
      val space       = searchSpace(strategy(trend))
      val wrongCount  = "Search genes do not match the number of active indicator leaves"
      val wrongShape  = "Search genes must be a flat Any composite, or None for an entirely fixed strategy"
      val wrongSchema = "Search genes have incompatible schemas"
      List(
        None                                                                                          -> wrongCount,
        Some(trend)                                                                                   -> wrongShape,
        Some(Indicator.compositeAnyOf(trend, trend))                                                  -> wrongCount,
        Some(Indicator.compositeAllOf(trend))                                                         -> wrongShape,
        Some(Indicator.compositeAnyOf(Indicator.compositeAnyOf(trend)))                               -> wrongSchema,
        Some(Indicator.compositeAnyOf(Indicator.TrendChangeDetection(ValueSource.Close, VT.EMA(20)))) -> wrongSchema
      ).foreach { case (genes, expectedError) =>
        space.restore(genes).left.map(_.getMessage) mustBe Left(expectedError)
      }
      succeed
    }

    "reject source, role, combinator, transformation type and sequence changes even inside fixed leaves" in {
      val basesAndChanged = List(
        trend                           -> Indicator.TrendChangeDetection(ValueSource.Open, VT.SMA(20)),
        trend                           -> Indicator.TrendChangeDetection(ValueSource.Close, VT.EMA(20)),
        raw                             -> Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.SMA(1)),
        raw                             -> Indicator.ValueTracking(ValueRole.Price, ValueSource.Open, VT.SMA(1)),
        Indicator.compositeAnyOf(trend) -> Indicator.compositeAllOf(trend),
        Indicator.compositeAnyOf(trend) -> Indicator.compositeAnyOf(trend, trend),
        Indicator.TrendChangeDetection(ValueSource.Close, VT.sequenced(VT.SMA(10), VT.EMA(20))) ->
          Indicator.TrendChangeDetection(ValueSource.Close, VT.sequenced(VT.SMA(10))),
        Indicator.TrendChangeDetection(ValueSource.Close, VT.NMA(20, 10, 0.5, MovingAverage.Simple)) ->
          Indicator.TrendChangeDetection(ValueSource.Close, VT.NMA(20, 10, 0.5, MovingAverage.Exponential)),
        Indicator.LinesCrossing(ValueSource.Close, VT.SMA(20), VT.EMA(50)) ->
          Indicator.LinesCrossing(ValueSource.Close, VT.EMA(50), VT.SMA(20)),
        Indicator.PriceLineCrossing(ValueSource.Close, ValueRole.ChannelMiddleBand, VT.SMA(20)) ->
          Indicator.PriceLineCrossing(ValueSource.Close, ValueRole.TrendStrength, VT.SMA(20))
      )
      basesAndChanged.foreach { case (base, changed) =>
        val space = searchSpace(strategy(base), Set(base))
        space.accepts(changed) mustBe false
        space.canonicalise(changed).left.map(_.getMessage) mustBe Left("Indicator does not match this round's search-space schema")
        space.project(changed).left.map(_.getMessage) mustBe Left("Indicator does not match this round's search-space schema")
      }
      succeed
    }

    "accept numeric variation while preserving a sequence's structure and NMA calculation method" in {
      val base    = Indicator.TrendChangeDetection(ValueSource.Close, VT.sequenced(VT.NMA(20, 10, 0.5, MovingAverage.Simple), VT.EMA(20)))
      val changed = Indicator.TrendChangeDetection(ValueSource.Close, VT.sequenced(VT.NMA(40, 15, 0.7, MovingAverage.Simple), VT.EMA(30)))
      val space   = searchSpace(strategy(base))
      space.accepts(changed) mustBe true
      space.canonicalise(changed) mustBe Right(changed)
      space.project(changed).flatMap(space.restore) mustBe Right(changed)
    }
  }

  "Tracked-value dependencies" should {
    val trackers = Indicator.Composite(NonEmptyList.fromListUnsafe(ValueRole.values.toList.map(tracker)), CombinationLogic.Any)

    def activeRoles(conditions: List[Rule.Condition]): Set[ValueRole] = {
      val space = searchSpace(strategy(trackers, conditions))
      leaves(space.project(trackers)).collect { case Indicator.ValueTracking(role, _, _) => role }.toSet
    }

    "keep every ValueIs role searchable through nested logical conditions" in {
      ValueRole.values.foreach { role =>
        val condition =
          Rule.Condition.Not(Rule.Condition.anyOf(Rule.Condition.allOf(Rule.Condition.ValueIs(role, Rule.Operator.LessThan, 10))))
        activeRoles(List(condition)) mustBe Set(role)
      }
      succeed
    }

    "keep the momentum direction and all velocity dependencies searchable" in {
      activeRoles(List(Rule.Condition.MomentumIs(Direction.Upward))) mustBe Set(ValueRole.Momentum)
      List(
        Rule.Condition.VelocityIs(Direction.Downward),
        Rule.Condition.VelocityIsBelow(0.1),
        Rule.Condition.VelocityCrossedLevel(0.0, Direction.Upward)
      ).foreach(condition => activeRoles(List(condition)) mustBe Set(ValueRole.Velocity))
      succeed
    }

    "keep both price and ATR for an adverse-price exit, including close rules" in {
      val base = TestStrategy(trackers, TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.PriceMovedAgainstEntry(2)))))
      val space = searchSpace(base)
      leaves(space.project(trackers)).collect { case Indicator.ValueTracking(role, _, _) => role }.toSet mustBe
        Set(ValueRole.Price, ValueRole.Volatility)
    }

    "distinguish event and zone conditions from tracked values with similar role names" in {
      val events = List(
        Rule.Condition.TrendChangedTo(Direction.Upward),
        Rule.Condition.TrendIs(Direction.Upward),
        Rule.Condition.TrendActiveFor(1.hour),
        Rule.Condition.CrossoverOccurred(Direction.Upward),
        Rule.Condition.MomentumEntered(MomentumZone.Neutral),
        Rule.Condition.MomentumIsIn(MomentumZone.Overbought),
        Rule.Condition.PreviousVolatilityIs(VolatilityRegime.High),
        Rule.Condition.VolatilityIs(VolatilityRegime.Low),
        Rule.Condition.PositionIs(TradeOrder.Position.Buy),
        Rule.Condition.PositionOpenFor(1.hour),
        Rule.Condition.NoPosition,
        Rule.Condition.UpperBandCrossed(Direction.Upward),
        Rule.Condition.LowerBandCrossed(Direction.Downward),
        Rule.Condition.PriceCrossedLine(ValueRole.Price, Direction.Upward)
      )
      activeRoles(events) mustBe Set.empty
    }
  }
}
