package currexx.backtest

import io.circe.Codec
import currexx.core.market.MomentumZone
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Direction, Indicator, ValueRole, ValueSource, ValueTransformation, VolatilityRegime}

import scala.concurrent.duration.*

final case class TestStrategy(
    indicator: Indicator,
    rules: TradeStrategy
) derives Codec.AsObject

object TestStrategy {

  // Trend-rider (NMA trend + STOCH). Its only original exit was momentum exhaustion, so losers ran
  // unbounded — median loss -0.0107 (~10x every other strategy), the price of its 10.6 W/L.
  // A 3*ATR price-distance stop (Turtle 2N-style, widened to 3N because 1.5N/2N choke the strategy)
  // caps that tail: median loss halved to -0.00494 and W/L stays healthy, at a ~29% cost to raw total
  // profit (0.30383 -> 0.21684). This is a risk/reward dial, not a free lunch — drop the stop to
  // recover raw profit at the cost of fatter tails. Requires ATR + Price trackers in the composite.
  // median win-to-loss ratio: 4.16520, total profit: 0.21684, total orders: 2005, median profit: 0.04796, median loss: -0.00494
  val s1 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation =
          ValueTransformation.NMA(length = 100, signalLength = 45, lambda = 0.8, maCalc = currexx.domain.signal.MovingAverage.Exponential)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.STOCH(length = 90),
        upperBoundary = 90.0,
        lowerBoundary = 11.0
      ),
      // ATR (absolute price units) feeds the price-distance stop via lastVolatilityValue.
      Indicator.ValueTracking(
        role = ValueRole.Volatility,
        source = ValueSource.Close,
        transformation = ValueTransformation.ATR(length = 14)
      ),
      // Latest close price feeds the stop via lastClosePrice (SMA(1) = identity).
      Indicator.ValueTracking(
        role = ValueRole.Price,
        source = ValueSource.Close,
        transformation = ValueTransformation.SMA(length = 1)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.MomentumEntered(MomentumZone.Neutral)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.MomentumEntered(MomentumZone.Neutral)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.momentumEnteredOverbought
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.momentumEnteredOversold
            ),
            // Risk stop: exit once price has moved 3*ATR against the entry.
            Rule.Condition.PriceMovedAgainstEntry(nAtr = 3.0)
          )
        )
      )
    )
  )

  // median win-to-loss ratio: 8.75000, total profit: 0.12653, total orders: 110, median profit: 0.02099, median loss: -0.0035966666666666663
  val s1_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // Primary signal: JMA crossover
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 43, phase = -67, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 16, phase = 45, power = 8)
      ),
      // Momentum filter
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16),
        upperBoundary = 50.0,
        lowerBoundary = 40.0
      ),
      // Momentum tracking
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16)
      ),
      // Volatility filter
      Indicator.VolatilityRegimeDetection(
        atrLength = 9,
        smoothingType = ValueTransformation.SMA(length = 5)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.upwardCrossover,
            Rule.Condition.volatilityIsLow,
            Rule.Condition.MomentumIs(Direction.Upward),
            Rule.Condition.Not(Rule.Condition.momentumIsInOverbought)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.downwardCrossover,
            Rule.Condition.volatilityIsLow,
            Rule.Condition.MomentumIs(Direction.Downward),
            Rule.Condition.Not(Rule.Condition.momentumIsInOversold)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.momentumEnteredOverbought
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.momentumEnteredOversold
            )
          )
        )
      )
    )
  )

  // median win-to-loss ratio: 5.901785, total profit: 0.20349, total orders: 652, median profit: 0.04662, median loss: -0.004795
  val s2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 43, phase = -67, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 16, phase = 45, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16),
        upperBoundary = 50.0,
        lowerBoundary = 44.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 9,
        smoothingType = ValueTransformation.SMA(length = 5)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.upwardCrossover,
            Rule.Condition.volatilityIsLow,
            Rule.Condition.Not(Rule.Condition.momentumIsInOverbought)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.downwardCrossover,
            Rule.Condition.volatilityIsLow,
            Rule.Condition.Not(Rule.Condition.momentumIsInOversold)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.momentumEnteredOverbought
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.momentumEnteredOversold
            )
          )
        )
      )
    )
  )

  // median win-to-loss ratio: 0.65220, total profit: 0.24328, total orders: 486, median profit: 0.03375, median loss: -0.00288
  val s4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // 1. Trend: JMA 50
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = 0, power = 2)
      ),
      // 2. Breakout Channel: Keltner Channel
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 20),
        atrLength = 20,
        atrMultiplier = 1.5
      ),
      // 3. Exit Momentum: RSX
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 85.0,
        lowerBoundary = 15.0
      ),
      // 4. Volatility
      Indicator.VolatilityRegimeDetection(
        atrLength = 14,
        smoothingType = ValueTransformation.SMA(length = 20)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,                   // Squeeze
            Rule.Condition.UpperBandCrossed(Direction.Upward) // Breakout
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,
            Rule.Condition.LowerBandCrossed(Direction.Downward)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.TrendChangedTo(Direction.Downward),
            Rule.Condition.TrendChangedTo(Direction.Upward),
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.momentumEnteredOverbought
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.momentumEnteredOversold
            )
          )
        )
      )
    )
  )

  // S4 with a proper trend-strength regime gate (ADX > 25 via the dedicated TrendStrength slot, which
  // does NOT collide with the momentum zone). Demonstrates the regime-filter infrastructure. NOTE:
  // the gate roughly halves trades (486 -> 258) but does NOT lift win rate (W/L 0.652 -> 0.641) and
  // costs ~10% total profit (0.24328 -> 0.21774) — i.e. on majors1h s4's losses are NOT concentrated
  // in low-ADX ranges, so the "false breakout" thesis is not supported here. Kept as a documented
  // reference for the regime filter; base s4 remains the stronger variant.
  // median win-to-loss ratio: 0.64140, total profit: 0.21774, total orders: 258, median profit: 0.03778, median loss: -0.00266
  val s4_regime = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = 0, power = 2)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 20),
        atrLength = 20,
        atrMultiplier = 1.5
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 85.0,
        lowerBoundary = 15.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 14,
        smoothingType = ValueTransformation.SMA(length = 20)
      ),
      // Trend-strength (ADX) tracked in its own slot; read by the ValueIs gate below.
      Indicator.ValueTracking(
        role = ValueRole.TrendStrength,
        source = ValueSource.Close,
        transformation = ValueTransformation.ADX(length = 14)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,
            Rule.Condition.UpperBandCrossed(Direction.Upward),
            Rule.Condition.ValueIs(ValueRole.TrendStrength, Rule.Operator.GreaterThan, 25.0) // regime gate
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,
            Rule.Condition.LowerBandCrossed(Direction.Downward),
            Rule.Condition.ValueIs(ValueRole.TrendStrength, Rule.Operator.GreaterThan, 25.0) // regime gate
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.TrendChangedTo(Direction.Downward),
            Rule.Condition.TrendChangedTo(Direction.Upward),
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.momentumEnteredOverbought
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.momentumEnteredOversold
            )
          )
        )
      )
    )
  )

  // median win-to-loss ratio: 4.16520, total profit: 0.11830, total orders: 416, median profit: 0.00723, median loss: -0.00524
  val s4_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = -67, power = 1)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 16),
        atrLength = 10,
        atrMultiplier = 1.5
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 74.0,
        lowerBoundary = 27.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 19,
        smoothingType = ValueTransformation.SMA(length = 5)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,                   // Squeeze
            Rule.Condition.UpperBandCrossed(Direction.Upward) // Breakout
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,
            Rule.Condition.LowerBandCrossed(Direction.Downward)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.TrendChangedTo(Direction.Downward),
            Rule.Condition.TrendChangedTo(Direction.Upward),
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.momentumEnteredOverbought
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.momentumEnteredOversold
            )
          )
        )
      )
    )
  )

  // median win-to-loss ratio: 3.204545, total profit: 0.29251, total orders: 564, median profit: 0.05418, median loss: -0.00333900349650349615
  val s5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 45, phase = 4, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 41),
        stdDevLength = 24,
        stdDevMultiplier = 2.8
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 13,
        smoothingType = ValueTransformation.SMA(length = 21)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 70.0,
        lowerBoundary = 37.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 20)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.anyOf(
              // 1. Breakout Entry (Trend Following)
              Rule.Condition.allOf(
                Rule.Condition.trendIsUpward,
                Rule.Condition.volatilityIsLow,                   // Squeeze
                Rule.Condition.UpperBandCrossed(Direction.Upward) // Bollinger Breakout
              ),
              // 2. Reversion Entry (Counter Trend / Deep Pullback)
              Rule.Condition.allOf(
                Rule.Condition.LowerBandCrossed(Direction.Upward),   // Price Re-enters Channel
                Rule.Condition.MomentumEntered(MomentumZone.Neutral) // Momentum turns up
              )
            )
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.anyOf(
              // 1. Breakout Entry
              Rule.Condition.allOf(
                Rule.Condition.trendIsDownward,
                Rule.Condition.volatilityIsLow,
                Rule.Condition.LowerBandCrossed(Direction.Downward)
              ),
              // 2. Reversion Entry
              Rule.Condition.allOf(
                Rule.Condition.UpperBandCrossed(Direction.Downward), // Price Re-enters Channel
                Rule.Condition.MomentumEntered(MomentumZone.Neutral) // Momentum turns down
              )
            )
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            // Trend-reversal stop, gated by position side so a favorable trend flip does not
            // close a winner (the ungated form closed longs on a flip-to-Upward and vice versa).
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.TrendChangedTo(Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.TrendChangedTo(Direction.Upward)
            ),
            // Take Profit: Momentum Extreme
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.momentumEnteredOverbought
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.momentumEnteredOversold
            )
          )
        )
      )
    )
  )

  // median win-to-loss ratio: 1.43939, total profit: 0.07022, total orders: 188, median profit: 0.013665, median loss: -0.00122187500000000005
  val s6_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 26),
        stdDevLength = 9,
        stdDevMultiplier = 1.4
      ),
      Indicator.ValueTracking(
        role = ValueRole.ChannelMiddleBand,
        source = ValueSource.Close,
        transformation = ValueTransformation.SMA(length = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.WilliamsR(length = 19),
        upperBoundary = 50.0,
        lowerBoundary = -80.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 9,
        smoothingType = ValueTransformation.SMA(length = 13)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.LowerBandCrossed(Direction.Upward),
            Rule.Condition.volatilityIsLow,
            Rule.Condition.Not(Rule.Condition.trendIsDownward),
            Rule.Condition.MomentumEntered(MomentumZone.Neutral)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.UpperBandCrossed(Direction.Downward),
            Rule.Condition.volatilityIsLow,
            Rule.Condition.Not(Rule.Condition.trendIsUpward),
            Rule.Condition.MomentumEntered(MomentumZone.Neutral)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Downward)
            ),
            Rule.Condition.PositionOpenFor(4.hours)
          )
        )
      )
    )
  )

  // Broken ADX ThresholdCrossing removed (it collided with the RSX ThresholdCrossing on the shared
  // momentum zone). Post-fix the RSX-only momentum semantics are correct; profit slips modestly
  // (0.06374 -> 0.05063), confirming the removed ADX was contributing via the zone-collision artifact
  // rather than as a genuine trend filter. A real gate is available via ValueIs(TrendStrength, ...).
  // median win-to-loss ratio: 0.77000, total profit: 0.05063, total orders: 193, median profit: 0.00830, median loss: -0.00154
  val s7_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = -16, power = 1)
      ),
      Indicator.ValueTracking(
        role = ValueRole.Velocity,
        source = ValueSource.HLC3,
        transformation = ValueTransformation.KalmanVelocity(gain = 0.16, measurementNoise = 0.06)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 75.0,
        lowerBoundary = 37.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 12,
        smoothingType = ValueTransformation.SMA(length = 36)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(4.hours),
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            Rule.Condition.VelocityCrossedLevel(level = 0.0005, direction = Direction.Upward)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(4.hours),
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            Rule.Condition.VelocityCrossedLevel(level = -0.0005, direction = Direction.Downward)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.VelocityCrossedLevel(level = -0.0003, direction = Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.VelocityCrossedLevel(level = 0.0003, direction = Direction.Upward)
            ),
            Rule.Condition.PositionOpenFor(8.hours),
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.TrendChangedTo(Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.TrendChangedTo(Direction.Upward)
            )
          )
        )
      )
    )
  )

  // median win-to-loss ratio: 0.88141, total profit: 0.20505, total orders: 279, median profit: 0.034605, median loss: -0.0018013166666666669
  val s8_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 41),
        atrLength = 7,
        atrMultiplier = 0.6
      ),
      Indicator.ValueTracking(
        role = ValueRole.Velocity,
        source = ValueSource.HLC3,
        transformation = ValueTransformation.KalmanVelocity(gain = 0.3, measurementNoise = 0.01)
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 23,
        smoothingType = ValueTransformation.SMA(length = 24)
      ),
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 26, phase = 42, power = 2)
      ),
      Indicator.PriceLineCrossing(
        source = ValueSource.Close,
        role = ValueRole.Momentum,
        transformation = ValueTransformation.ParabolicSAR(afStart = 0.025, afMax = 0.13, afStep = 0.015)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.PreviousVolatilityIs(VolatilityRegime.Low),
            Rule.Condition.volatilityIsHigh,
            Rule.Condition.UpperBandCrossed(Direction.Upward),
            Rule.Condition.VelocityIs(Direction.Upward)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.PreviousVolatilityIs(VolatilityRegime.Low),
            Rule.Condition.volatilityIsHigh,
            Rule.Condition.LowerBandCrossed(Direction.Downward),
            Rule.Condition.VelocityIs(Direction.Downward)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.PriceCrossedLine(ValueRole.Momentum, Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.PriceCrossedLine(ValueRole.Momentum, Direction.Upward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.TrendChangedTo(Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.TrendChangedTo(Direction.Upward)
            )
          )
        )
      )
    )
  )

  // median win-to-loss ratio: 1.430485, total profit: 0.21718, total orders: 255, median profit: 0.043995, median loss: -0.0014979761904761906
  val s9_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.PriceLineCrossing(
        source = ValueSource.Close,
        role = ValueRole.ChannelMiddleBand,
        transformation = ValueTransformation.IchimokuKijunSen(length = 44)
      ),
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.IchimokuKijunSen(length = 23)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.CCI(length = 24),
        upperBoundary = 95.0,
        lowerBoundary = 17.0
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 14),
        stdDevLength = 10,
        stdDevMultiplier = 1.4
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            Rule.Condition.LowerBandCrossed(Direction.Upward)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            Rule.Condition.UpperBandCrossed(Direction.Downward)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.TrendChangedTo(Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.TrendChangedTo(Direction.Upward)
            ),
            Rule.Condition.PositionOpenFor(6.hours)
          )
        )
      )
    )
  )

  // Broken ADX ThresholdCrossing removed. IMPORTANT: the previously recorded 0.22800 profit was
  // largely an artifact of the ADX momentum-zone collision — the GA had tuned ADX(8, 61/36) which,
  // arriving after the RSX ThresholdCrossing, WON the shared momentum zone, so the entry trigger
  // `momentumEnteredOverbought` was really firing on ADX crossings, not RSX. Removing the broken ADX
  // to restore correct RSX semantics drops profit to 0.16896 (1845 -> 1731 orders). That ~26% is the
  // portion of s10_optimized's edge that came from the bug rather than genuine signal. A correct
  // trend-strength gate (ValueIs(TrendStrength, ADX) > 20/30) only reduces it further (0.123 / 0.046),
  // so no gate is wired here. Still the best surviving momentum-continuation variant.
  // median win-to-loss ratio: 0.88370, total profit: 0.16896, total orders: 1731, median profit: 0.02698, median loss: -0.00068
  val s10_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 24, phase = 32, power = 9)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 5),
        upperBoundary = 54.0,
        lowerBoundary = 11.0
      ),
      // ADX ThresholdCrossing removed — it corrupted the shared momentum zone (see banner above).
      Indicator.VolatilityRegimeDetection(
        atrLength = 6,
        smoothingType = ValueTransformation.SMA(length = 16)
      ),
      Indicator.ValueTracking(
        role = ValueRole.Velocity,
        source = ValueSource.HLC3,
        transformation = ValueTransformation.KalmanVelocity(gain = 0.25, measurementNoise = 0.02)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,
            Rule.Condition.momentumEnteredOverbought,
            Rule.Condition.VelocityIs(Direction.Upward)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,
            Rule.Condition.momentumEnteredOversold,
            Rule.Condition.VelocityIs(Direction.Downward)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.PositionOpenFor(2.hours),
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.TrendChangedTo(Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.TrendChangedTo(Direction.Upward)
            )
          )
        )
      )
    )
  )

  // S11: Volume-Confirmed Breakout (CMF)
  // Only trade breakouts that have institutional volume behind them.
  // CMF > 0 confirms buying pressure for longs; CMF < 0 confirms selling pressure for shorts.
  // Avoids fake-outs where price breaks a level but volume doesn't participate.
  // median win-to-loss ratio: 1.20441, total profit: 0.08671, total orders: 225, median profit: 0.010675, median loss: -0.00146237318840579735
  val s11 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // Trend detection
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 40, phase = 0, power = 2)
      ),
      // Keltner Channel for breakout detection
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 20),
        atrLength = 14,
        atrMultiplier = 2.3
      ),
      // CMF for volume confirmation — tracked as Momentum role for threshold access
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.CMF(length = 20),
        upperBoundary = 0.05,
        lowerBoundary = -0.05
      ),
      // Volatility filter
      Indicator.VolatilityRegimeDetection(
        atrLength = 14,
        smoothingType = ValueTransformation.SMA(length = 20)
      ),
      // Parabolic SAR for trailing exit
      Indicator.PriceLineCrossing(
        source = ValueSource.Close,
        role = ValueRole.Momentum,
        transformation = ValueTransformation.ParabolicSAR(afStart = 0.02, afMax = 0.2, afStep = 0.02)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,
            // Keltner breakout = entry trigger
            Rule.Condition.UpperBandCrossed(Direction.Upward),
            // CMF is in buying-pressure zone = volume confirms the breakout (state filter, not a fresh cross)
            Rule.Condition.momentumIsInOverbought
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,
            // Keltner breakdown = entry trigger
            Rule.Condition.LowerBandCrossed(Direction.Downward),
            // CMF is in selling-pressure zone = volume confirms the breakdown (state filter, not a fresh cross)
            Rule.Condition.momentumIsInOversold
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            // Parabolic SAR flip — adaptive trailing stop
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.PriceCrossedLine(ValueRole.Momentum, Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.PriceCrossedLine(ValueRole.Momentum, Direction.Upward)
            ),
            // Trend reversal
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.TrendChangedTo(Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.TrendChangedTo(Direction.Upward)
            ),
            // Time cap — volume edge decays
            Rule.Condition.PositionOpenFor(4.hours)
          )
        )
      )
    )
  )

  // median win-to-loss ratio: 1.319005, total profit: 0.06493, total orders: 215, median profit: 0.007155, median loss: -0.00156749999999999995
  val s11_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 47, phase = 36, power = 3)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 17),
        atrLength = 15,
        atrMultiplier = 1.2
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.CMF(length = 14),
        upperBoundary = 0.05,
        lowerBoundary = -0.05
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 16,
        smoothingType = ValueTransformation.SMA(length = 26)
      ),
      Indicator.PriceLineCrossing(
        source = ValueSource.Close,
        role = ValueRole.Momentum,
        transformation = ValueTransformation.ParabolicSAR(afStart = 0.03, afMax = 0.1, afStep = 0.03)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,
            Rule.Condition.UpperBandCrossed(Direction.Upward),
            Rule.Condition.momentumEnteredOverbought
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow,
            Rule.Condition.LowerBandCrossed(Direction.Downward),
            Rule.Condition.momentumEnteredOversold
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.PriceCrossedLine(ValueRole.Momentum, Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.PriceCrossedLine(ValueRole.Momentum, Direction.Upward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.TrendChangedTo(Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.TrendChangedTo(Direction.Upward)
            ),
            Rule.Condition.PositionOpenFor(4.hours)
          )
        )
      )
    )
  )

  // S12: CMF Trend Confirmation
  // Enter when CMF confirms trend direction — buying pressure aligns with uptrend, selling pressure with downtrend.
  // CMF threshold cross acts as the primary entry trigger; Ichimoku Kijun-Sen provides trend context.
  // Trend + low-volatility filters screen out ranging markets. Ride each position until the Ichimoku
  // trend actually reverses — no trailing stop, no time cap.
  //
  // EXIT REDESIGN: the original s12 exited on a Parabolic SAR flip (afMax=0.3, very aggressive) OR a
  // trend reversal, giving W/L 0.74 and total profit 0.06177 over majors1h. The SAR and every time-cap
  // variant tested were amputating the fat tail of winning trend trades. Exiting ONLY on a position-gated
  // Ichimoku trend reversal lifts total profit to 0.28206, W/L to 1.22355, and cuts orders 490 -> 156
  // (5 of 6 majors profitable). The Parabolic SAR indicator was consequently removed as dead weight.
  // median win-to-loss ratio: 1.22355, total profit: 0.28206, total orders: 156, median profit: 0.04487, median loss: -0.00756
  val s12 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.IchimokuKijunSen(length = 26)
      ),
      // CMF is the sole momentum-zone driver (a second ThresholdCrossing would collide on the
      // single shared momentum slot — see ADX removal note below).
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.CMF(length = 20),
        upperBoundary = 0.17,
        lowerBoundary = -0.17
      ),
      // NOTE: an ADX ThresholdCrossing was removed here. ThresholdCrossing indicators all write the
      // single shared `momentum` zone, so ADX silently overwrote/corrupted the CMF signal that the
      // rules read via momentumEntered*, and was never consumed as a filter. Trend + volatility
      // filters below already screen out ranging markets.
      Indicator.VolatilityRegimeDetection(
        atrLength = 14,
        smoothingType = ValueTransformation.SMA(length = 20)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.momentumEnteredOverbought, // CMF crossed above +0.17
            Rule.Condition.volatilityIsLow
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.momentumEnteredOversold, // CMF crossed below -0.17
            Rule.Condition.volatilityIsLow
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            // Ride the trend; exit only when the Ichimoku trend reverses against the position.
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.TrendChangedTo(Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.TrendChangedTo(Direction.Upward)
            )
          )
        )
      )
    )
  )
}
