package currexx.backtest

import currexx.core.market.MomentumZone
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.market.TradeOrder
import currexx.domain.signal.{Direction, Indicator, ValueSource, ValueTransformation}

import scala.concurrent.duration.*

object Strategy {

  val s1Indicators = List(
    // The primary trend indicator remains the same.
    Indicator.TrendChangeDetection(
      source = ValueSource.HLC3,
      transformation = ValueTransformation.Kalman(gain = 0.05)
    ),
    // We need a momentum indicator to act as a filter.
    Indicator.ThresholdCrossing(
      source = ValueSource.Close,
      transformation = ValueTransformation.RSX(length = 14),
      upperBoundary = 70.0,
      lowerBoundary = 30.0
    )
  )

  val s1Rules = TradeStrategy(
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

  val s2Indicators = List(
    // The crossover indicator remains the same.
    Indicator.LinesCrossing(
      source = ValueSource.Close,
      line1Transformation = ValueTransformation.EMA(length = 21), // Assuming FAST
      line2Transformation = ValueTransformation.EMA(length = 55)  // Assuming SLOW
    ),
    // The momentum indicator remains the same.
    Indicator.ThresholdCrossing(
      source = ValueSource.Close,
      transformation = ValueTransformation.RSX(length = 14),
      upperBoundary = 75.0,
      lowerBoundary = 25.0
    ),
    // NEW: A trend indicator to provide a faster exit signal.
    Indicator.TrendChangeDetection(
      source = ValueSource.HLC3,
      transformation = ValueTransformation.Kalman(gain = 0.08) // A medium-speed Kalman
    )
  )

  val s2Rules = TradeStrategy(
    openRules = List(
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.AllOf(
          List(
            Rule.Condition.NoPosition,
            // The trigger is a "golden cross": fast line (line1) crosses above slow line (line2)
            Rule.Condition.CrossoverOccurred(Direction.Upward),
            // Filter: don't buy if already overbought.
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
            // Faster Stop-Loss - Exit as soon as our Kalman trend filter turns down.
            Rule.Condition.TrendChangedTo(Direction.Downward),
            // Take Profit Rule (same as before) - Exit when momentum gets exhausted.
            Rule.Condition.MomentumEntered(MomentumZone.Overbought)
          )
        )
      )
    )
  )

  val s3Indicators = List(
    Indicator.TrendChangeDetection(
      source = ValueSource.HLC3,
      transformation = ValueTransformation.JMA(length = 50, phase = 0, power = 2)
    ),
    Indicator.ThresholdCrossing(
      source = ValueSource.Close,
      transformation = ValueTransformation.STOCH(length = 14), // Stochastic is great for pullbacks
      upperBoundary = 80.0,
      lowerBoundary = 20.0
    )
  )

  val s3Rules = TradeStrategy(
    // The strategy now has rules for both opening long and short positions.
    openRules = List(
      // Rule for opening a LONG position
      Rule(
        action = TradeAction.OpenLong,
        conditions = Rule.Condition.AllOf(
          List(
            Rule.Condition.NoPosition,
            Rule.Condition.TrendIs(Direction.Upward),
            Rule.Condition.TrendActiveFor(12.hours),
            Rule.Condition.AnyOf(
              List(
                Rule.Condition.MomentumEntered(MomentumZone.Neutral),
                Rule.Condition.AllOf(
                  List(
                    Rule.Condition.MomentumIs(Direction.Upward),
                    Rule.Condition.Not(Rule.Condition.MomentumIsIn(MomentumZone.Overbought))
                  )
                )
              )
            )
          )
        )
      ),
      // Rule for opening a SHORT position
      Rule(
        action = TradeAction.OpenShort,
        conditions = Rule.Condition.AllOf(
          List(
            Rule.Condition.NoPosition,
            Rule.Condition.TrendIs(Direction.Downward),
            Rule.Condition.TrendActiveFor(12.hours),
            Rule.Condition.AnyOf(
              List(
                Rule.Condition.MomentumEntered(MomentumZone.Neutral),
                Rule.Condition.AllOf(
                  List(
                    Rule.Condition.MomentumIs(Direction.Downward),
                    Rule.Condition.Not(Rule.Condition.MomentumIsIn(MomentumZone.Oversold))
                  )
                )
              )
            )
          )
        )
      )
    ),
    // The close rules can often be symmetrical for both long and short positions.
    closeRules = List(
      // Exit Rule 1: Stop-loss if the primary trend fails (reverses against us).
      Rule(
        action = TradeAction.ClosePosition,
        conditions = Rule.Condition.AnyOf(
          List(
            Rule.Condition.AllOf(
              List(                                                 // For an open LONG position
                Rule.Condition.PositionIs(TradeOrder.Position.Buy), // Assuming you have this condition
                Rule.Condition.TrendChangedTo(Direction.Downward)
              )
            ),
            Rule.Condition.AllOf(
              List( // For an open SHORT position
                Rule.Condition.PositionIs(TradeOrder.Position.Sell),
                Rule.Condition.TrendChangedTo(Direction.Upward)
              )
            )
          )
        )
      ),
      // Exit Rule 2: Take-profit if momentum becomes exhausted.
      Rule(
        action = TradeAction.ClosePosition,
        conditions = Rule.Condition.AnyOf(
          List(
            Rule.Condition.AllOf(
              List( // For an open LONG position
                Rule.Condition.PositionIs(TradeOrder.Position.Buy),
                Rule.Condition.MomentumEntered(MomentumZone.Overbought)
              )
            ),
            Rule.Condition.AllOf(
              List( // For an open SHORT position
                Rule.Condition.PositionIs(TradeOrder.Position.Sell),
                Rule.Condition.MomentumEntered(MomentumZone.Oversold)
              )
            )
          )
        )
      )
    )
  )
}
