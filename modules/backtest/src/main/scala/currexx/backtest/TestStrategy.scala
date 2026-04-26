package currexx.backtest

import io.circe.Codec
import currexx.core.market.MomentumZone
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Direction, Indicator, ValueRole, ValueSource, ValueTransformation}

import scala.concurrent.duration.*

final case class TestStrategy(
    indicator: Indicator,
    rules: TradeStrategy
) derives Codec.AsObject

object TestStrategy {

  // median win-to-loss ratio: 1.50425, total profit: 0.41534, total orders: 2879, median profit: 0.07230, median loss: -0.00157
  val s1 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.HMA(length = 25)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.STOCH(length = 14),
        upperBoundary = 83.0,
        lowerBoundary = 23.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.STOCH(length = 14)
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 10,
        smoothingType = ValueTransformation.SMA(length = 20)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        // Rule for LONG positions
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsHigh,
            // The original logic was good, and now it's robust because we have ValueTracking.
            Rule.Condition.anyOf(
              Rule.Condition.MomentumEntered(MomentumZone.Neutral),
              Rule.Condition.MomentumIs(Direction.Upward)
            ),
            Rule.Condition.Not(Rule.Condition.momentumIsInOverbought)
          )
        ),
        // Symmetrical rule for SHORT positions
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsHigh,
            Rule.Condition.anyOf(
              Rule.Condition.MomentumEntered(MomentumZone.Neutral),
              Rule.Condition.MomentumIs(Direction.Downward)
            ),
            Rule.Condition.Not(Rule.Condition.momentumIsInOversold)
          )
        )
      ),
      // SIMPLIFIED close rules that don't depend on knowing the position direction.
      // This is a more general and often more robust approach.
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            // Exit if momentum enters an extreme zone opposite to the presumed trade.
            // E.g., if we are long, this triggers when we become overbought.
            // If we are short, this triggers when we become oversold.
            Rule.Condition.momentumEnteredOverbought,
            Rule.Condition.momentumEnteredOversold,
            // Exit if the primary trend flips against us. This acts as a master stop-loss.
            Rule.Condition.TrendChangedTo(Direction.Downward),
            Rule.Condition.TrendChangedTo(Direction.Upward)
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

  // median win-to-loss ratio: 8.07143, total profit: 0.14133, total orders: 380, median profit: 0.036145, median loss: -0.004103
  val s2_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 43, phase = -67, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 16, phase = 35, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16),
        upperBoundary = 51.0,
        lowerBoundary = 44.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 6,
        smoothingType = ValueTransformation.SMA(length = 4)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.upwardCrossover,
            Rule.Condition.volatilityIsLow,
            Rule.Condition.MomentumIsIn(MomentumZone.Neutral)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.downwardCrossover,
            Rule.Condition.volatilityIsLow,
            Rule.Condition.MomentumIsIn(MomentumZone.Neutral)
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

  // median win-to-loss ratio: 0.580955, total profit: -0.01253, total orders: 230, median profit: -0.00369, median loss: -0.001455
  val s3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // 1. The TREND filter: A slow Kalman filter on the price.
      // This will populate the `MarketProfile.trend` state.
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.Kalman(gain = 0.3, measurementNoise = 0.4) // Slow and smooth
      ),

      // 2. The VELOCITY tracker: A faster Kalman filter that tracks velocity.
      // This will populate `MarketProfile.lastTrendVelocity`.
      Indicator.ValueTracking(
        role = ValueRole.Velocity, // Requires adding this new role
        source = ValueSource.HLC3,
        transformation = ValueTransformation.KalmanVelocity(gain = 0.4, measurementNoise = 0.05) // Faster and more responsive
      ),

      // 3. The EXIT filter: A momentum oscillator for take-profit signals.
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 85.0,
        lowerBoundary = 15.0
      ),
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
            // Prerequisites:
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(2.hours), // CONFIRMATION: The trend must be established for at least 4 hours.
            Rule.Condition.volatilityIsLow,         // FILTER: Only enter during low-volatility periods.

            // The Entry Trigger:
            // Velocity must surge above a symmetrical positive threshold.
            // This confirms a real breakout in momentum.
            Rule.Condition.VelocityCrossedLevel(level = 0.0012, direction = Direction.Upward)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            // Prerequisites:
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(2.hours), // CONFIRMATION
            Rule.Condition.volatilityIsLow,         // FILTER

            // The Entry Trigger:
            // Velocity must break below a symmetrical negative threshold.
            Rule.Condition.VelocityCrossedLevel(level = -0.0012, direction = Direction.Downward)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            // --- ADAPTIVE STOP-LOSS: The momentum has died ---
            // For a long position, we exit if velocity is no longer positive.
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.VelocityCrossedLevel(level = -0.0002, direction = Direction.Downward)
            ),
            // For a short position, we exit if velocity is no longer negative.
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.VelocityCrossedLevel(level = 0.0002, direction = Direction.Upward)
            ),

            // --- TAKE-PROFIT: Momentum is exhausted ---
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

  // median win-to-loss ratio: 0.62435, total profit: 0.17587, total orders: 448, median profit: 0.02513, median loss: -0.00291
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
            Rule.Condition.TrendActiveFor(2.hours),
            Rule.Condition.volatilityIsLow, // Squeeze
            Rule.Condition.UpperBandCrossed(Direction.Upward) // Breakout
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(2.hours),
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

  // median win-to-loss ratio: 0.835085, total profit: 0.26284, total orders: 423, median profit: 0.03555, median loss: -0.002855
  val s5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // 1. Trend: JMA 50
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = 0, power = 2)
      ),
      // 2. Breakout Channel: Bollinger Bands (Standard 2.0 SD)
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 20),
        stdDevLength = 20,
        stdDevMultiplier = 2.0
      ),
      // 3. Volatility
      Indicator.VolatilityRegimeDetection(
        atrLength = 14,
        smoothingType = ValueTransformation.SMA(length = 20)
      ),
      // 4. Momentum Signal (RSX)
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 80.0,
        lowerBoundary = 20.0
      ),
      // 5. Momentum Tracking (RSX)
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14)
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
                Rule.Condition.volatilityIsLow, // Squeeze
                Rule.Condition.UpperBandCrossed(Direction.Upward) // Bollinger Breakout
              ),
              // 2. Reversion Entry (Counter Trend / Deep Pullback)
              Rule.Condition.allOf(
                Rule.Condition.LowerBandCrossed(Direction.Upward), // Price Re-enters Channel
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
            Rule.Condition.TrendChangedTo(Direction.Downward),
            Rule.Condition.TrendChangedTo(Direction.Upward),
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

  val s1_balanced_optimised = s1.copy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.HMA(length = 28)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.STOCH(length = 17),
        upperBoundary = 86.0,
        lowerBoundary = 21.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.STOCH(length = 30)
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 8,
        smoothingType = ValueTransformation.SMA(length = 11)
      )
    )
  )

  val s2_balanced_optimised = s2.copy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 21, phase = -2, power = 4),
        line2Transformation = ValueTransformation.JMA(length = 10, phase = -55, power = 4)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 13),
        upperBoundary = 67.0,
        lowerBoundary = 28.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 33,
        smoothingType = ValueTransformation.SMA(length = 55)
      )
    )
  )

  val s2v2_wl_ratio_optimised = s2_v2.copy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 30, phase = 8, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 31, power = 10)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 57.0,
        lowerBoundary = 41.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 23,
        smoothingType = ValueTransformation.SMA(length = 5)
      )
    )
  )

  val s2v2_risk_adjusted_optimised = s2_v2.copy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 23, phase = 17, power = 3),
        line2Transformation = ValueTransformation.JMA(length = 22, phase = 4, power = 2)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 20),
        upperBoundary = 73.0,
        lowerBoundary = 27.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 17,
        smoothingType = ValueTransformation.SMA(length = 40)
      )
    )
  )

  val s3_balanced_optimised = s3.copy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.Kalman(gain = 0.25, measurementNoise = 0.01)
      ),
      Indicator.ValueTracking(
        role = ValueRole.Velocity,
        source = ValueSource.HLC3,
        transformation = ValueTransformation.KalmanVelocity(gain = 0.48, measurementNoise = 0.02)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 70.0,
        lowerBoundary = 29.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 11,
        smoothingType = ValueTransformation.SMA(length = 31)
      )
    )
  )

  val s4_profit_optimised = s4.copy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 31, phase = -88, power = 3)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 26),
        atrLength = 49,
        atrMultiplier = 1.0
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 22),
        upperBoundary = 78.0,
        lowerBoundary = 12.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 37,
        smoothingType = ValueTransformation.SMA(length = 30)
      )
    )
  )

  val s5_balanced_optimised = s5.copy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 28, phase = -100, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 26),
        stdDevLength = 33,
        stdDevMultiplier = 1.9
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 21,
        smoothingType = ValueTransformation.SMA(length = 17)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 71.0,
        lowerBoundary = 30.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 19)
      )
    )
  )
}
