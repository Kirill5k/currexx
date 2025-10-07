package currexx.backtest

import currexx.core.market.MomentumZone
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Direction, Indicator, ValueRole, ValueSource, ValueTransformation, VolatilityRegime}

import scala.concurrent.duration.*

object TestStrategy {

  val s1_indicator = Indicator.compositeAnyOf(
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

  val s1_rules = TradeStrategy(
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
  val s2_indicator = Indicator.compositeAnyOf(
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
  val s2_rules = TradeStrategy(
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

  val s3_indicator = Indicator.compositeAnyOf(
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
      smoothingType = ValueTransformation.SMA(length = 20),
      smoothingLength = 20
    )
  )

  val s3_rules = TradeStrategy(
    openRules = List(
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.allOf(
          // Prerequisites:
          Rule.Condition.NoPosition,
          Rule.Condition.trendIsUpward,
          Rule.Condition.TrendActiveFor(4.hours), // CONFIRMATION: The trend must be established for at least 4 hours.
          Rule.Condition.volatilityIsLow,         // FILTER: Only enter during low-volatility periods.

          // The Entry Trigger:
          // Velocity must surge above a symmetrical positive threshold.
          // This confirms a real breakout in momentum.
          Rule.Condition.VelocityCrossedLevel(level = 0.0003, direction = Direction.Upward)
        )
      ),
      Rule(
        action = TradeAction.OpenShort,
        conditions = Rule.Condition.allOf(
          // Prerequisites:
          Rule.Condition.NoPosition,
          Rule.Condition.trendIsDownward,
          Rule.Condition.TrendActiveFor(4.hours), // CONFIRMATION
          Rule.Condition.volatilityIsLow,         // FILTER

          // The Entry Trigger:
          // Velocity must break below a symmetrical negative threshold.
          Rule.Condition.VelocityCrossedLevel(level = -0.0003, direction = Direction.Downward)
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
            Rule.Condition.VelocityCrossedLevel(level = -0.0001, direction = Direction.Downward)
          ),
          // For a short position, we exit if velocity is no longer negative.
          Rule.Condition.allOf(
            Rule.Condition.positionIsSell,
            Rule.Condition.VelocityCrossedLevel(level = 0.0001, direction = Direction.Upward)
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

  val s1_indicators_v2 = List(
    // 1. Trend Indicator (Unchanged)
    Indicator.TrendChangeDetection(
      source = ValueSource.HLC3,
      transformation = ValueTransformation.JMA(length = 29, phase = 60, power = 3)
    ),

    // 2. Momentum Indicator (Unchanged)
    Indicator.ThresholdCrossing(
      source = ValueSource.Close,
      transformation = ValueTransformation.STOCH(length = 14),
      upperBoundary = 80.0,
      lowerBoundary = 20.0
    ),

    // 3. Continuous Value Tracker for Momentum (Unchanged)
    Indicator.ValueTracking(
      role = ValueRole.Momentum,
      source = ValueSource.Close,
      transformation = ValueTransformation.STOCH(length = 14)
    ),

    // 4. NEW: Volatility Regime Filter
    Indicator.VolatilityRegimeDetection(
      atrLength = 14,
      smoothingType = ValueTransformation.SMA(20),
      smoothingLength = 20
    ),

    Indicator.PriceLineCrossing(
      source = ValueSource.Close,
      role = ValueRole.ChannelMiddleBand,
      transformation = ValueTransformation.EMA(20)
    ),
  )

  val s1_rules_v2 = TradeStrategy(
    openRules = List(
      // --- Rule for LONG positions ---
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.allOf(
          // Prerequisites
          Rule.Condition.NoPosition,
          Rule.Condition.trendIsUpward,
          Rule.Condition.TrendActiveFor(12.hours),

          // NEW: VOLATILITY FILTER - Only take trades in calm, predictable markets.
          Rule.Condition.VolatilityIs(VolatilityRegime.Low),

          // Pullback Entry Logic (same as before, but now safer)
          Rule.Condition.anyOf(
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            Rule.Condition.allOf(
              Rule.Condition.MomentumIs(Direction.Upward),
              Rule.Condition.Not(Rule.Condition.momentumIsInOverbought)
            )
          )
        )
      ),
      // --- Symmetrical rule for SHORT positions ---
      Rule(
        action = TradeAction.OpenShort,
        conditions = Rule.Condition.allOf(
          Rule.Condition.NoPosition,
          Rule.Condition.trendIsDownward,
          Rule.Condition.TrendActiveFor(12.hours),
          Rule.Condition.VolatilityIs(VolatilityRegime.Low), // Apply filter to shorts as well
          Rule.Condition.anyOf(
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            Rule.Condition.allOf(
              Rule.Condition.MomentumIs(Direction.Downward),
              Rule.Condition.Not(Rule.Condition.momentumIsInOversold)
            )
          )
        )
      )
    ),
    closeRules = List(
      Rule(
        action = TradeAction.ClosePosition,
        conditions = Rule.Condition.anyOf(
          // --- MASTER STOP-LOSS: The primary trend flips against us ---
          Rule.Condition.allOf(Rule.Condition.positionIsBuy, Rule.Condition.TrendChangedTo(Direction.Downward)),
          Rule.Condition.allOf(Rule.Condition.positionIsSell, Rule.Condition.TrendChangedTo(Direction.Upward)),

          // --- NEW, DYNAMIC TAKE-PROFIT / TRAILING STOP ---
          // Exit a long position if the price closes back below the Keltner middle band.
          // This allows the trade to profit from the full extent of a strong trend.
          Rule.Condition.allOf(
            Rule.Condition.positionIsBuy,
            Rule.Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Downward)
          ),
          // Symmetrical exit for a short position.
          Rule.Condition.allOf(
            Rule.Condition.positionIsSell,
            Rule.Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward)
          )
        )
      )
    )
  )
}
