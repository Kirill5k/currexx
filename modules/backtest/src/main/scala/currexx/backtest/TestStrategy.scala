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
      transformation = ValueTransformation.Kalman(gain = 0.05)
    ),
    // Momentum events (entering Overbought/Oversold)
    Indicator.ThresholdCrossing(
      source = ValueSource.Close,
      transformation = ValueTransformation.RSX(length = 14),
      upperBoundary = 70.0,
      lowerBoundary = 30.0
    ),
    // NEW: Explicitly track the RSX value on every bar
    Indicator.ValueTracking(
      role = ValueRole.Momentum, // Use the type-safe enum
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
      transformation = ValueTransformation.JMA(length = 29, phase = 100, power = 3)
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
}
