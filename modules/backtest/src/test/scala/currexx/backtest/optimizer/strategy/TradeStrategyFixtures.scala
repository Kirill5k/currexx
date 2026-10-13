package currexx.backtest.optimizer.strategy

import currexx.core.market.MomentumZone
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.market.TradeOrder
import currexx.domain.signal.{Direction, Indicator, ValueRole, ValueSource, ValueTransformation as VT, VolatilityRegime}

import scala.concurrent.duration.*

private[strategy] object TradeStrategyFixtures:
  import Rule.Condition as C
  import RuleParameterBounds.{DoubleRange, DurationRange}

  val indicator: Indicator = Indicator.compositeAnyOf(
    Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(20)),
    Indicator.LinesCrossing(ValueSource.Close, VT.SMA(10), VT.SMA(20)),
    Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(10), 70, 30),
    Indicator.VolatilityRegimeDetection(14, VT.SMA(20)),
    Indicator.BollingerBands(ValueSource.Close, VT.SMA(20), 20, 2.0),
    Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(10)),
    Indicator.ValueTracking(ValueRole.Velocity, ValueSource.Close, VT.KalmanVelocity(0.1, 0.1)),
    Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1)),
    Indicator.ValueTracking(ValueRole.Volatility, ValueSource.Close, VT.ATR(14)),
    Indicator.PriceLineCrossing(ValueSource.Close, ValueRole.Price, VT.SMA(10)),
    Indicator.PriceLineCrossing(ValueSource.Close, ValueRole.ChannelMiddleBand, VT.SMA(20))
  )

  val conditions: List[C] = List(
    C.TrendChangedTo(Direction.Upward),
    C.TrendIs(Direction.Upward),
    C.TrendActiveFor(3.hours),
    C.CrossoverOccurred(Direction.Upward),
    C.MomentumEntered(MomentumZone.Neutral),
    C.MomentumIsIn(MomentumZone.Oversold),
    C.MomentumIs(Direction.Upward),
    C.PreviousVolatilityIs(VolatilityRegime.High),
    C.VolatilityIs(VolatilityRegime.Low),
    C.VelocityIs(Direction.Upward),
    C.VelocityCrossedLevel(0.1, Direction.Upward),
    C.VelocityIsBelow(0.2),
    C.PositionIs(TradeOrder.Position.Buy),
    C.PositionOpenFor(12.hours),
    C.NoPosition,
    C.UpperBandCrossed(Direction.Upward),
    C.LowerBandCrossed(Direction.Downward),
    C.PriceCrossedLine(ValueRole.Price, Direction.Upward),
    C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50.0),
    C.PriceMovedAgainstEntry(4.0)
  )

  val template: TradeStrategy = TradeStrategy(List(Rule(TradeAction.OpenLong, C.AllOf(conditions))), Nil)

  def path(index: Int): RulePath = RulePath(RuleList.Open, 0, Vector(index))

  def at(index: Int, parameter: RuleParameter): RuleTarget = RuleTarget.At(RuleParameterPath(path(index), parameter), template)

  val bounds: RuleParameterBounds = RuleParameterBounds(
    numeric = Map(
      at(10, RuleParameter.Level)     -> DoubleRange(-1, 1, 0.1),
      at(11, RuleParameter.Threshold) -> DoubleRange(0, 1, 0.1),
      at(18, RuleParameter.Level)     -> DoubleRange(0, 100, 1),
      RuleTarget.AtrStop              -> DoubleRange(1, 8, 0.25)
    ),
    durations = Map(
      RuleTarget.TrendAge    -> DurationRange(0.hours, 24.hours, 1.hour),
      RuleTarget.HoldingTime -> DurationRange(1.hour, 72.hours, 1.hour)
    )
  )

  val space: TradeStrategySearchSpace = TradeStrategySearchSpace.forStrategy(template, indicator, bounds).fold(throw _, identity)

  def single(condition: C): TradeStrategy = TradeStrategy(List(Rule(TradeAction.OpenLong, condition)), Nil)

  def changed(index: Int, condition: C): TradeStrategy = single(C.AllOf(conditions.updated(index, condition)))

  def mirrored(trendAge: FiniteDuration = 3.hours, holdingTime: FiniteDuration = 12.hours, atr: Double = 4.0): TradeStrategy =
    TradeStrategy(
      List(
        Rule(TradeAction.OpenLong, C.AllOf(List(C.TrendIs(Direction.Upward), C.TrendActiveFor(trendAge)))),
        Rule(TradeAction.OpenShort, C.AllOf(List(C.TrendIs(Direction.Downward), C.Not(C.TrendActiveFor(trendAge)))))
      ),
      List(
        Rule(
          TradeAction.ClosePosition,
          C.AnyOf(
            List(
              C.AllOf(List(C.PositionIs(TradeOrder.Position.Buy), C.PositionOpenFor(holdingTime), C.PriceMovedAgainstEntry(atr))),
              C.AllOf(List(C.PositionIs(TradeOrder.Position.Sell), C.PositionOpenFor(holdingTime), C.PriceMovedAgainstEntry(atr)))
            )
          )
        )
      )
    )

  val linkedBounds: RuleParameterBounds = RuleParameterBounds(
    numeric = Map(RuleTarget.AtrStop -> DoubleRange(1, 8, 0.25)),
    durations = Map(
      RuleTarget.TrendAge    -> DurationRange(0.hours, 24.hours, 1.hour),
      RuleTarget.HoldingTime -> DurationRange(1.hour, 72.hours, 1.hour)
    )
  )
