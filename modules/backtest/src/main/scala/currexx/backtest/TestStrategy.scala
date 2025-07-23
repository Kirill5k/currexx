package currexx.backtest

import currexx.core.market.MomentumZone
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Direction, Indicator, ValueRole, ValueSource, ValueTransformation}

import scala.concurrent.duration.*

object TestStrategy {

  val s1_indicators = List(
    // Trend detection - no changes needed here.
    Indicator.TrendChangeDetection(
      source = ValueSource.HLC3,
      transformation = ValueTransformation.Kalman(gain = 0.05, measurementNoise = 1.0)
    ),
    // Momentum events (entering Overbought/Oversold)
    Indicator.ThresholdCrossing(
      source = ValueSource.Close,
      transformation = ValueTransformation.RSX(length = 14),
      upperBoundary = 70.0,
      lowerBoundary = 30.0
    ),
    Indicator.ValueTracking(
      role = ValueRole.Momentum,
      source = ValueSource.Close,
      transformation = ValueTransformation.RSX(length = 14)
    )
  )

  val s1_rules = TradeStrategy(
    openRules = List(
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.allOf(
          Rule.Condition.NoPosition,
          Rule.Condition.trendIsUpward,
          Rule.Condition.TrendActiveFor(4.hours),
          Rule.Condition.Not(Rule.Condition.momentumIsInOverbought)
        )
      )
    ),
    closeRules = List(
      Rule(
        action = TradeAction.ClosePosition,
        conditions = Rule.Condition.anyOf(
          Rule.Condition.TrendChangedTo(Direction.Downward),
          Rule.Condition.momentumEnteredOverbought
        )
      )
    )
  )

  val s2_indicators = List(
    Indicator.LinesCrossing(
      source = ValueSource.Close,
      // For a "Golden Cross" buy signal, line1 should be the FAST MA
      line1Transformation = ValueTransformation.EMA(length = 21), // FAST
      line2Transformation = ValueTransformation.EMA(length = 55)  // SLOW
    ),
    Indicator.ThresholdCrossing(
      source = ValueSource.Close,
      transformation = ValueTransformation.RSX(length = 14),
      upperBoundary = 75.0,
      lowerBoundary = 25.0
    ),
    // Explicitly track the momentum value
    Indicator.ValueTracking(
      role = ValueRole.Momentum,
      source = ValueSource.Close,
      transformation = ValueTransformation.RSX(length = 14)
    ),
    // The trend indicator for the exit is also fine.
    Indicator.TrendChangeDetection(
      source = ValueSource.HLC3,
      transformation = ValueTransformation.Kalman(gain = 0.08, measurementNoise = 1.0)
    )
  )

  val s2_rules = TradeStrategy(
    openRules = List(
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.allOf(
          Rule.Condition.NoPosition,
          Rule.Condition.upwardCrossover,
          Rule.Condition.not(Rule.Condition.momentumIsInOverbought)
        )
      )
    ),
    closeRules = List(
      Rule(
        action = TradeAction.ClosePosition,
        conditions = Rule.Condition.anyOf(
          Rule.Condition.TrendChangedTo(Direction.Downward),
          Rule.Condition.momentumEnteredOverbought
        )
      )
    )
  )

  val s3_indicators = List(
    Indicator.TrendChangeDetection(
      source = ValueSource.HLC3,
      transformation = ValueTransformation.JMA(length = 29, phase = 60, power = 3)
    ),
    Indicator.ThresholdCrossing(
      source = ValueSource.Close,
      transformation = ValueTransformation.STOCH(length = 14),
      upperBoundary = 80.0,
      lowerBoundary = 20.0
    ),
    // CRITICAL: We MUST track the Stochastic value to use MomentumIs
    Indicator.ValueTracking(
      role = ValueRole.Momentum,
      source = ValueSource.Close,
      transformation = ValueTransformation.STOCH(length = 14)
    )
  )

  val s3_rules = TradeStrategy(
    openRules = List(
      // Rule for LONG positions
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.allOf(
          Rule.Condition.NoPosition,
          Rule.Condition.trendIsUpward,
          Rule.Condition.TrendActiveFor(12.hours),
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
          Rule.Condition.TrendActiveFor(12.hours),
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

  // --- The Set of Indicators for the Filtered JMA Crossover Strategy ---
  val s4_indicators = List(
    // The primary crossover indicator using two different JMAs.
    Indicator.LinesCrossing(
      source = ValueSource.HLC3, // Use a smooth price source for the JMAs
      // The FAST line: short length, positive phase for responsiveness
      line1Transformation = ValueTransformation.JMA(length = 7, phase = 50, power = 2),
      // The SLOW line: longer length, negative phase for extreme smoothness
      line2Transformation = ValueTransformation.JMA(length = 30, phase = -50, power = 2)
    ),

    // The momentum filter indicator.
    Indicator.ThresholdCrossing(
      source = ValueSource.Close,
      transformation = ValueTransformation.RSX(length = 14),
      upperBoundary = 80.0, // Defines the Overbought zone
      lowerBoundary = 20.0  // Defines the Oversold zone
    ),

    // The volatility filter indicator.
    Indicator.VolatilityRegimeDetection(
      atrLength = 14,
      smoothingType = ValueTransformation.SMA(length = 20), // Compare ATR to its 20-period SMA
      smoothingLength = 20
    )
  )

  // --- The Full TradeStrategy Definition for Filtered JMA Crossover ---
  val s4_rules =   TradeStrategy(
    // openRules are now the primary drivers for both entries and reversals.
    openRules = List(
      // Rule for being LONG
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.allOf(
          // Entry Trigger: A "Golden Cross" of the JMAs.
          Rule.Condition.upwardCrossover,

          // FILTERS: These apply to both initial entries and reversals.
          Rule.Condition.volatilityIsLow,
          Rule.Condition.Not(Rule.Condition.momentumIsInOverbought)
        )
      ),

      // Rule for being SHORT
      Rule(
        action = TradeAction.OpenShort,
        conditions = Rule.Condition.allOf(
          // Entry Trigger: A "Death Cross" of the JMAs.
          Rule.Condition.downwardCrossover,

          // FILTERS:
          Rule.Condition.volatilityIsLow,
          Rule.Condition.Not(Rule.Condition.momentumIsInOversold)
        )
      )
    ),

    // closeRules are now for non-reversing exits to a FLAT state.
    // The crossover logic has been REMOVED from here.
    closeRules = List(
      Rule(
        action = TradeAction.ClosePosition,
        conditions = Rule.Condition.anyOf(
          // Emergency Exit 1: Volatility explodes. Get out and wait for calm.
          Rule.Condition.volatilityIsHigh,

          // Emergency Exit 2 (Take Profit): Momentum becomes completely exhausted.
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

  // 0.3 / 0.4 - total profit: 0.32679
  // 0.01 / 0.2 - total profit: 0.37519,
  val s5_indicators = List(
    // 1. The TREND filter: A slow Kalman filter on the price.
    // This will populate the `MarketProfile.trend` state.
    Indicator.TrendChangeDetection(
      source = ValueSource.HLC3,
      transformation = ValueTransformation.Kalman(gain = 0.01, measurementNoise = 0.2) // Slow and smooth
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
    )
  )

  val s5_rules = TradeStrategy(
    openRules = List(
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.allOf(
          // Prerequisites:
          Rule.Condition.NoPosition,
          Rule.Condition.trendIsUpward, // The slow Kalman filter must confirm an uptrend.

          // The Entry Trigger:
          // Velocity must cross ABOVE a positive threshold. This confirms a
          // real breakout in momentum, not just noise around the zero line.
          Rule.Condition.VelocityCrossedLevel(level = 0.0005, direction = Direction.Upward)
        )
      ),
      Rule(
        action = TradeAction.OpenShort,
        conditions = Rule.Condition.allOf(
          // Prerequisites:
          Rule.Condition.NoPosition,
          Rule.Condition.trendIsDownward,

          // The Entry Trigger:
          // Velocity must cross BELOW a negative threshold.
          Rule.Condition.VelocityCrossedLevel(level = -0.0005, direction = Direction.Downward)
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
            Rule.Condition.VelocityCrossedLevel(level = -0.02, direction = Direction.Downward)
          ),
          // For a short position, we exit if velocity is no longer negative.
          Rule.Condition.allOf(
            Rule.Condition.positionIsSell,
            Rule.Condition.VelocityCrossedLevel(level = 0.02, direction = Direction.Upward)
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
}
