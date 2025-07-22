package currexx.backtest

import currexx.core.market.MomentumZone
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.market.TradeOrder
import currexx.domain.signal.{Direction, Indicator, ValueRole, ValueSource, ValueTransformation, VolatilityRegime}

import scala.concurrent.duration.*

object TestStrategy {

  val s1_indicators = List(
    // Trend detection - no changes needed here.
    Indicator.TrendChangeDetection(
      source = ValueSource.HLC3,
      transformation = ValueTransformation.Kalman(gain = 0.05)
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
        conditions = Rule.Condition.AllOf(
          List(
            Rule.Condition.NoPosition,
            Rule.Condition.TrendIs(Direction.Upward),
            Rule.Condition.TrendActiveFor(4.hours),
            Rule.Condition.Not(Rule.Condition.MomentumIsIn(MomentumZone.Overbought))
          )
        )
      )
    ),
    closeRules = List(
      Rule(
        action = TradeAction.ClosePosition,
        conditions = Rule.Condition.AnyOf(
          List(
            Rule.Condition.TrendChangedTo(Direction.Downward),
            Rule.Condition.MomentumEntered(MomentumZone.Overbought)
          )
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
      transformation = ValueTransformation.Kalman(gain = 0.08)
    )
  )

  val s2_rules = TradeStrategy(
    openRules = List(
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.AllOf(
          List(
            Rule.Condition.NoPosition,
            Rule.Condition.CrossoverOccurred(Direction.Upward),
            Rule.Condition.Not(Rule.Condition.MomentumIsIn(MomentumZone.Overbought))
          )
        )
      )
    ),
    closeRules = List(
      Rule(
        action = TradeAction.ClosePosition,
        conditions = Rule.Condition.AnyOf(
          List(
            Rule.Condition.TrendChangedTo(Direction.Downward),
            Rule.Condition.MomentumEntered(MomentumZone.Overbought)
          )
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
        conditions = Rule.Condition.AllOf(List(
          Rule.Condition.NoPosition,
          Rule.Condition.TrendIs(Direction.Upward),
          Rule.Condition.TrendActiveFor(12.hours),
          // The original logic was good, and now it's robust because we have ValueTracking.
          Rule.Condition.AnyOf(List(
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            Rule.Condition.MomentumIs(Direction.Upward)
          )),
          Rule.Condition.Not(Rule.Condition.MomentumIsIn(MomentumZone.Overbought))
        ))
      ),
      // Symmetrical rule for SHORT positions
      Rule(
        action = TradeAction.OpenShort,
        conditions = Rule.Condition.AllOf(List(
          Rule.Condition.NoPosition,
          Rule.Condition.TrendIs(Direction.Downward),
          Rule.Condition.TrendActiveFor(12.hours),
          Rule.Condition.AnyOf(List(
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            Rule.Condition.MomentumIs(Direction.Downward)
          )),
          Rule.Condition.Not(Rule.Condition.MomentumIsIn(MomentumZone.Oversold))
        ))
      )
    ),
    // SIMPLIFIED close rules that don't depend on knowing the position direction.
    // This is a more general and often more robust approach.
    closeRules = List(
      Rule(
        action = TradeAction.ClosePosition,
        conditions = Rule.Condition.AnyOf(List(
          // Exit if momentum enters an extreme zone opposite to the presumed trade.
          // E.g., if we are long, this triggers when we become overbought.
          // If we are short, this triggers when we become oversold.
          Rule.Condition.MomentumEntered(MomentumZone.Overbought),
          Rule.Condition.MomentumEntered(MomentumZone.Oversold),
          // Exit if the primary trend flips against us. This acts as a master stop-loss.
          Rule.Condition.TrendChangedTo(Direction.Downward),
          Rule.Condition.TrendChangedTo(Direction.Upward)
        ))
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
      lowerBoundary = 20.0 // Defines the Oversold zone
    ),

    // The volatility filter indicator.
    Indicator.VolatilityRegimeDetection(
      atrLength = 14,
      smoothingType = ValueTransformation.SMA(length = 20), // Compare ATR to its 20-period SMA
      smoothingLength = 20
    )
  )

  // --- The Full TradeStrategy Definition for Filtered JMA Crossover ---
  val s4_rules = TradeStrategy(
    openRules = List(
      // --- Rule for Opening a LONG Position ---
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.AllOf(List(
          // Prerequisite Checks:
          Rule.Condition.NoPosition,
          Rule.Condition.VolatilityIs(VolatilityRegime.Low), // FILTER 1: Only trade in low volatility
          Rule.Condition.Not(Rule.Condition.MomentumIsIn(MomentumZone.Overbought)), // FILTER 2: Don't buy if overbought

          // The Entry Trigger:
          // A "Golden Cross" of the JMAs (fast line crosses above slow line)
          Rule.Condition.CrossoverOccurred(Direction.Upward)
        ))
      ),

      // --- Rule for Opening a SHORT Position ---
      Rule(
        action = TradeAction.OpenShort,
        conditions = Rule.Condition.AllOf(List(
          // Prerequisite Checks:
          Rule.Condition.NoPosition,
          Rule.Condition.VolatilityIs(VolatilityRegime.Low), // FILTER 1
          Rule.Condition.Not(Rule.Condition.MomentumIsIn(MomentumZone.Oversold)), // FILTER 2

          // The Entry Trigger:
          // A "Death Cross" of the JMAs (fast line crosses below slow line)
          Rule.Condition.CrossoverOccurred(Direction.Downward)
        ))
      )
    ),

    closeRules = List(
      // A single, comprehensive rule for closing any open position.
      Rule(
        action = TradeAction.ClosePosition,
        conditions = Rule.Condition.AnyOf(List(
          // --- STOP-LOSS Condition: A reverse crossover occurs ---
          Rule.Condition.AllOf(List(
            Rule.Condition.PositionIs(TradeOrder.Position.Buy), // If we are long...
            Rule.Condition.CrossoverOccurred(Direction.Downward) // ...exit on a death cross.
          )),
          Rule.Condition.AllOf(List(
            Rule.Condition.PositionIs(TradeOrder.Position.Sell), // If we are short...
            Rule.Condition.CrossoverOccurred(Direction.Upward) // ...exit on a golden cross.
          )),

          // --- TAKE-PROFIT Condition: Momentum becomes exhausted ---
          Rule.Condition.AllOf(List(
            Rule.Condition.PositionIs(TradeOrder.Position.Buy), // If we are long...
            Rule.Condition.MomentumEntered(MomentumZone.Overbought) // ...exit when momentum is overbought.
          )),
          Rule.Condition.AllOf(List(
            Rule.Condition.PositionIs(TradeOrder.Position.Sell), // If we are short...
            Rule.Condition.MomentumEntered(MomentumZone.Oversold) // ...exit when momentum is oversold.
          ))
        ))
      )
    )
  )
}
