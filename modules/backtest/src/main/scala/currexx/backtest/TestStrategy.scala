package currexx.backtest

import io.circe.Codec
import currexx.core.market.MomentumZone
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Direction, Indicator, MovingAverage, ValueRole, ValueSource, ValueTransformation}

import scala.concurrent.duration.*

final case class TestStrategy(
    indicator: Indicator,
    rules: TradeStrategy
) derives Codec.AsObject

object TestStrategy {

  // net=5849.53626, closed=2231, forced=4, win=81.22%, exp=2.621935, PF=1.300, DD=1.34%, Sharpe=3.005, gross=8033.44066, costs=2183.9044
  val s1_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.NMA(length = 50, signalLength = 45, lambda = 1.0, maCalc = MovingAverage.Exponential)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.STOCH(length = 32),
        upperBoundary = 90.0,
        lowerBoundary = 39.0
      ),
      // ATR (absolute price units) feeds the price-distance stop via lastVolatilityValue.
      Indicator.ValueTracking(
        role = ValueRole.Volatility,
        source = ValueSource.Close,
        transformation = ValueTransformation.ATR(length = 33)
      ),
      // Close price feeds the stop via lastClosePrice.
      Indicator.ValueTracking(
        role = ValueRole.Price,
        source = ValueSource.Close,
        transformation = ValueTransformation.SMA(length = 5)
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

  // GA-optimized indicator params for s1_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1706-s1_optimized_shuffle.md (training 1.329574 -> validation 0.284857, shuffled GA).
  // BREACHES 4 constraint(s) on validation data:
  //   - profitable pair-months is 0.525, required >= 0.550
  //   - most concentrated pair's best month is 0.754, required <= 0.700
  //   - profit factor is 1.11679, required >= 1.2
  //   - costs as a share of gross profit is 0.450, required <= 0.400
  // net=3903.15282, closed=2211, forced=4, win=77.75%, exp=1.765334, PF=1.207, DD=1.50%, Sharpe=2.140, gross=6067.49973, costs=2164.34692
  val s1_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.NMA(length = 35, signalLength = 17, lambda = 4.0, maCalc = MovingAverage.Exponential)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.STOCH(length = 22),
        upperBoundary = 70.0,
        lowerBoundary = 16.0
      ),
      // ATR (absolute price units) feeds the price-distance stop via lastVolatilityValue.
      Indicator.ValueTracking(
        role = ValueRole.Volatility,
        source = ValueSource.Close,
        transformation = ValueTransformation.ATR(length = 38)
      ),
      // Close price feeds the stop via lastClosePrice.
      Indicator.ValueTracking(
        role = ValueRole.Price,
        source = ValueSource.Close,
        transformation = ValueTransformation.SMA(length = 28)
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

  // GA-optimized indicator params for s1_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1654-s1_optimized.md (training 1.578097 -> validation 0.000850).
  // BREACHES 5 constraint(s) on validation data:
  //   - most concentrated pair's best month is 0.953, required <= 0.700
  //   - pair-month profit factor is 1.138749975674175696447109352018543, required >= 1.3
  //   - profit factor is 1.03100, required >= 1.2
  //   - costs as a share of gross profit is 0.737, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // net=3249.49950, closed=2148, forced=2, win=80.54%, exp=1.512802, PF=1.174, DD=1.79%, Sharpe=2.031, gross=5352.16753, costs=2102.66803
  val s1_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.NMA(length = 50, signalLength = 45, lambda = 1.0, maCalc = MovingAverage.Exponential)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.STOCH(length = 27),
        upperBoundary = 83.0,
        lowerBoundary = 39.0
      ),
      // ATR (absolute price units) feeds the price-distance stop via lastVolatilityValue.
      Indicator.ValueTracking(
        role = ValueRole.Volatility,
        source = ValueSource.Close,
        transformation = ValueTransformation.ATR(length = 25)
      ),
      // Close price feeds the stop via lastClosePrice.
      Indicator.ValueTracking(
        role = ValueRole.Price,
        source = ValueSource.Close,
        transformation = ValueTransformation.SMA(length = 19)
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

  // net=1141.97611, closed=109, forced=0, win=86.24%, exp=10.476845, PF=3.174, DD=0.27%, Sharpe=2.919, gross=1248.19274, costs=106.21663
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

  // GA-optimized indicator params for s1_v2 (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1729-s1_v2_shuffle.md (training 2.114148 -> validation 0.461305, shuffled GA).
  // BREACHES 2 constraint(s) on validation data:
  //   - profitable pair-months is 0.548, required >= 0.550
  //   - most concentrated pair's best month is 0.867, required <= 0.700
  // net=6606.82211, closed=190, forced=6, win=54.21%, exp=34.772748, PF=2.179, DD=1.64%, Sharpe=2.511, gross=6795.13611, costs=188.31400
  val s1_v2_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // Primary signal: JMA crossover
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 19, phase = 7, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 9, phase = -26, power = 5)
      ),
      // Momentum filter
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 43),
        upperBoundary = 94.0,
        lowerBoundary = 11.0
      ),
      // Momentum tracking
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 9)
      ),
      // Volatility filter
      Indicator.VolatilityRegimeDetection(
        atrLength = 27,
        smoothingType = ValueTransformation.SMA(length = 31)
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

  // GA-optimized indicator params for s1_v2, re-optimized from s1_v2_optimized (rules unchanged). Best
  // Top-25 member from ga-optimisation-2026-07-14-2343-s1_v2_optimized.md (fitness 0.957191).
  // net=8544.76614, closed=184, forced=6, win=65.76%, exp=46.438946, PF=3.148, DD=0.77%, Sharpe=2.940, gross=8727.12305, costs=182.35692
  val s1_v2_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // Primary signal: JMA crossover
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 40, phase = -55, power = 4),
        line2Transformation = ValueTransformation.JMA(length = 19, phase = -1, power = 9)
      ),
      // Momentum filter
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 28),
        upperBoundary = 72.0,
        lowerBoundary = 14.0
      ),
      // Momentum tracking
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6)
      ),
      // Volatility filter
      Indicator.VolatilityRegimeDetection(
        atrLength = 19,
        smoothingType = ValueTransformation.SMA(length = 55)
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

  // GA-optimized indicator params for s1_v2_optimized_v2 (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1738-s1_v2_optimized_v2.md (training 2.097416 -> validation 1.227990).
  // Satisfies every constraint on validation data.
  // net=7331.48928, closed=195, forced=6, win=60.51%, exp=37.597381, PF=2.476, DD=0.95%, Sharpe=3.008, gross=7526.09515, costs=194.60586
  val s1_v2_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // Primary signal: JMA crossover
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 40, phase = -55, power = 4),
        line2Transformation = ValueTransformation.JMA(length = 19, phase = -4, power = 9)
      ),
      // Momentum filter
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 77.0,
        lowerBoundary = 16.0
      ),
      // Momentum tracking
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10)
      ),
      // Volatility filter
      Indicator.VolatilityRegimeDetection(
        atrLength = 17,
        smoothingType = ValueTransformation.SMA(length = 68)
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

  // GA-optimized indicator params for s1_v2_optimized_v2 (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1746-s1_v2_optimized_v2_shuffle.md (training 2.184967 -> validation 0.075746, shuffled GA).
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.452, required >= 0.550
  //   - most concentrated pair's best month is 0.886, required <= 0.700
  //   - profitable datasets is 0.333, required >= 0.667
  // net=5337.34128, closed=263, forced=6, win=53.99%, exp=20.294073, PF=1.777, DD=1.89%, Sharpe=2.302, gross=5596.33276, costs=258.99148
  val s1_v2_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // Primary signal: JMA crossover
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 24, phase = -37, power = 3),
        line2Transformation = ValueTransformation.JMA(length = 5, phase = -53, power = 2)
      ),
      // Momentum filter
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 30),
        upperBoundary = 89.0,
        lowerBoundary = 15.0
      ),
      // Momentum tracking
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16)
      ),
      // Volatility filter
      Indicator.VolatilityRegimeDetection(
        atrLength = 24,
        smoothingType = ValueTransformation.SMA(length = 37)
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

  // net=1700.76417, closed=643, forced=1, win=79.32%, exp=2.645045, PF=1.335, DD=0.94%, Sharpe=1.966, gross=2333.10508, costs=632.34091
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

  // GA-optimized indicator params for s2 (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1801-s2_shuffle.md (training 1.937273 -> validation 1.313772, shuffled GA).
  // BREACHES 1 constraint(s) on validation data:
  //   - most concentrated pair's best month is 0.704, required <= 0.700
  // net=6231.29154, closed=775, forced=6, win=41.29%, exp=8.040376, PF=1.564, DD=1.18%, Sharpe=4.117, gross=6973.32715, costs=742.03561
  val s2_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 19, phase = 17, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 36, phase = -48, power = 2)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 23),
        upperBoundary = 86.0,
        lowerBoundary = 14.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 41,
        smoothingType = ValueTransformation.SMA(length = 31)
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

  // GA-optimized indicator params for s2 (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1755-s2.md (training 2.047228 -> validation 0.063279).
  // BREACHES 3 constraint(s) on validation data:
  //   - pair-month profit factor is 1.224584358662623739024413542690434, required >= 1.3
  //   - profit factor is 1.08474, required >= 1.2
  //   - profitable datasets is 0.333, required >= 0.667
  // net=4882.14619, closed=602, forced=3, win=71.26%, exp=8.109877, PF=1.623, DD=1.65%, Sharpe=2.765, gross=5467.55058, costs=585.40439
  val s2_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 38, phase = -41, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 23, phase = 33, power = 6)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16),
        upperBoundary = 59.0,
        lowerBoundary = 13.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 9,
        smoothingType = ValueTransformation.SMA(length = 7)
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

  // GA-optimized indicator params for s2, re-optimized from s2_optimized (rules unchanged). Best Top-25
  // member from ga-optimisation-2026-07-15-0052-s2_optimized.md (fitness 0.687735).
  // net=4594.75298, closed=190, forced=5, win=70.53%, exp=24.182910, PF=1.956, DD=1.13%, Sharpe=2.536, gross=4780.60292, costs=185.84994
  val s2_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 35, phase = -58, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 44, phase = 90, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 31),
        upperBoundary = 59.0,
        lowerBoundary = 31.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 10,
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

  // GA-optimized indicator params for s2_optimized_v2 (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1814-s2_optimized_v2_shuffle.md (training 1.905028 -> validation 0.787786, shuffled GA).
  // BREACHES 1 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  // net=5929.23463, closed=630, forced=6, win=38.73%, exp=9.411484, PF=1.571, DD=1.13%, Sharpe=3.004, gross=6544.48371, costs=615.24908
  val s2_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 23, phase = 89, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 25, phase = -96, power = 2)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 23),
        upperBoundary = 75.0,
        lowerBoundary = 9.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 43,
        smoothingType = ValueTransformation.SMA(length = 34)
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

  // net=3559.10491, closed=258, forced=0, win=75.19%, exp=13.794980, PF=2.467, DD=0.60%, Sharpe=3.479, gross=3811.91334, costs=252.80842
  val s4_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = -98, power = 1)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 21),
        atrLength = 11,
        atrMultiplier = 2.3
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 11),
        upperBoundary = 75.0,
        lowerBoundary = 29.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 31,
        smoothingType = ValueTransformation.SMA(length = 37)
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

  // GA-optimized indicator params for s4_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1820-s4_optimized.md (training 1.658671 -> validation 1.675786).
  // Satisfies every constraint on validation data.
  // net=3649.62245, closed=225, forced=3, win=76.44%, exp=16.220544, PF=2.899, DD=0.46%, Sharpe=4.488, gross=3870.90817, costs=221.28573
  val s4_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = -73, power = 1)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 21),
        atrLength = 10,
        atrMultiplier = 2.4
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 11),
        upperBoundary = 75.0,
        lowerBoundary = 29.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 31,
        smoothingType = ValueTransformation.SMA(length = 35)
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

  // GA-optimized indicator params for s4_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1831-s4_optimized_shuffle.md (training 1.754503 -> validation 0.011446, shuffled GA).
  // BREACHES 6 constraint(s) on validation data:
  //   - profitable pair-months is 0.421, required >= 0.550
  //   - most concentrated pair's best month is 0.872, required <= 0.700
  //   - pair-month profit factor is 1.199613810303675485740318798590868, required >= 1.3
  //   - profit factor is 1.09087, required >= 1.2
  //   - costs as a share of gross profit is 0.437, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // net=2215.86261, closed=485, forced=2, win=54.43%, exp=4.568789, PF=1.428, DD=1.26%, Sharpe=1.743, gross=2687.49935, costs=471.63674
  val s4_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 44, phase = -91, power = 2)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 37),
        atrLength = 11,
        atrMultiplier = 2.3
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 12),
        upperBoundary = 63.0,
        lowerBoundary = 17.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 25,
        smoothingType = ValueTransformation.SMA(length = 31)
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

  // net=2931.48571, closed=164, forced=0, win=78.05%, exp=17.874913, PF=4.105, DD=0.22%, Sharpe=3.795, gross=3091.53751, costs=160.05179
  val s4_regime_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 45, phase = -25, power = 1)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 24),
        atrLength = 27,
        atrMultiplier = 1.6
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 22),
        upperBoundary = 67.0,
        lowerBoundary = 35.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 19,
        smoothingType = ValueTransformation.SMA(length = 55)
      ),
      Indicator.ValueTracking(
        role = ValueRole.TrendStrength,
        source = ValueSource.Close,
        transformation = ValueTransformation.ADX(length = 23)
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

  // GA-optimized indicator params for s4_regime_optimized_v2 (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1853-s4_regime_optimized_v2_shuffle.md (training 1.907076 -> validation 0.620178, shuffled GA).
  // BREACHES 1 constraint(s) on validation data:
  //   - profitable pair-months is 0.444, required >= 0.550
  // net=1600.56427, closed=134, forced=0, win=57.46%, exp=11.944509, PF=2.423, DD=0.25%, Sharpe=3.499, gross=1730.48851, costs=129.92423
  val s4_regime_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 41, phase = -61, power = 2)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 28),
        atrLength = 20,
        atrMultiplier = 1.6
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 27),
        upperBoundary = 62.0,
        lowerBoundary = 27.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 27,
        smoothingType = ValueTransformation.SMA(length = 54)
      ),
      Indicator.ValueTracking(
        role = ValueRole.TrendStrength,
        source = ValueSource.Close,
        transformation = ValueTransformation.ADX(length = 31)
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

  // GA-optimized indicator params for s5, re-optimized from the s5 baseline (rules unchanged). Best
  // Top-25 member from ga-optimisation-2026-07-17-1858-s5.md (fitness 0.539268).
  // net=2897.64330, closed=187, forced=0, win=72.19%, exp=15.495419, PF=2.658, DD=0.45%, Sharpe=2.853, gross=3078.85870, costs=181.21540
  val s5_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = 54, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 43),
        stdDevLength = 38,
        stdDevMultiplier = 2.8
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 13,
        smoothingType = ValueTransformation.SMA(length = 39)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 15),
        upperBoundary = 63.0,
        lowerBoundary = 31.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 5)
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
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.TrendChangedTo(Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.TrendChangedTo(Direction.Upward)
            ),
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
  // net=2306.49103, closed=153, forced=5, win=50.33%, exp=15.075105, PF=1.417, DD=1.95%, Sharpe=1.816, gross=2457.28396, costs=150.79293
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

  // GA-optimized indicator params for s12 (rules unchanged). Best Top-25 member from
  // ga-optimisation-2026-07-05-1349-s12.md (fitness 0.407640).
  // net=3181.94074, closed=172, forced=5, win=51.74%, exp=18.499655, PF=1.555, DD=1.69%, Sharpe=2.346, gross=3351.48927, costs=169.54853
  val s12_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.IchimokuKijunSen(length = 26)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.CMF(length = 11),
        upperBoundary = 0.17,
        lowerBoundary = -0.17
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 28,
        smoothingType = ValueTransformation.SMA(length = 44)
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
