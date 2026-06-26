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

  // median win-to-loss ratio: 10.60000, total profit: 0.30383, total orders: 1085, median profit: 0.04417, median loss: -0.0107199999999999985
  val s1 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.NMA(length = 100, signalLength = 45, lambda = 0.8, maCalc = currexx.domain.signal.MovingAverage.Exponential)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.STOCH(length = 90),
        upperBoundary = 90.0,
        lowerBoundary = 11.0
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
            )
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

  // median win-to-loss ratio: 6.055555, total profit: 0.00710, total orders: 393, median profit: 0.005585, median loss: -0.0066399999999999995
  val s1_v2_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 47, phase = -46, power = 3),
        line2Transformation = ValueTransformation.JMA(length = 17, phase = 52, power = 3)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 12),
        upperBoundary = 58.0,
        lowerBoundary = 45.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 20)
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 12,
        smoothingType = ValueTransformation.SMA(length = 6)
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
  
  // median win-to-loss ratio: 10.183335, total profit: 0.15742, total orders: 371, median profit: 0.034035, median loss: -0.0043565
  val s2_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 42, phase = -67, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 16, phase = 31, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16),
        upperBoundary = 51.0,
        lowerBoundary = 45.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 5,
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
            Rule.Condition.MomentumEntered(MomentumZone.Neutral)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.downwardCrossover,
            Rule.Condition.volatilityIsLow,
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
            )
          )
        )
      )
    )
  )
  
  // median win-to-loss ratio: 1.08737, total profit: 0.07452, total orders: 562, median profit: 0.01272, median loss: -0.0012252694444444444
  val s3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.Kalman(gain = 0.47, measurementNoise = 0.05)
      ),
      Indicator.ValueTracking(
        role = ValueRole.Velocity,
        source = ValueSource.HLC3,
        transformation = ValueTransformation.KalmanVelocity(gain = 0.3, measurementNoise = 0.02)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 5),
        upperBoundary = 72.0,
        lowerBoundary = 23.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 17,
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
            Rule.Condition.TrendActiveFor(1.hour), // CONFIRMATION: The trend must be established for at least 4 hours.
            Rule.Condition.volatilityIsLow, // FILTER: Only enter during low-volatility periods.

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
            Rule.Condition.TrendActiveFor(1.hour), // CONFIRMATION
            Rule.Condition.volatilityIsLow, // FILTER

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
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.volatilityIsLow, // Squeeze
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

  // median win-to-loss ratio: 4.42121, total profit: 0.12925, total orders: 408, median profit: 0.008535, median loss: -0.0052235314685314685
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
            Rule.Condition.volatilityIsLow, // Squeeze
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
  
  // median win-to-loss ratio: 3.204545, total profit: 0.28762, total orders: 564, median profit: 0.05399, median loss: -0.00333900349650349615
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

  // S6: Pure Mean Reversion (Bollinger Bounce)
  // Trade price returning to the mean after extreme band touches in ranging markets.
  // Uses Williams %R (faster than RSX for reversal timing) and ADX to confirm ranging market.
  //median win-to-loss ratio: 0.78788, total profit: -0.11061, total orders: 289, median profit: -0.01632, median loss: -0.00174467914438502625
  val s6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 20),
        stdDevLength = 20,
        stdDevMultiplier = 2.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.ChannelMiddleBand,
        source = ValueSource.Close,
        transformation = ValueTransformation.SMA(length = 20)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.WilliamsR(length = 14),
        upperBoundary = -20.0,
        lowerBoundary = -80.0
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
            // Take profit at middle band
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Downward)
            ),
            // Time stop
            Rule.Condition.PositionOpenFor(4.hours)
          )
        )
      )
    )
  )

  //median win-to-loss ratio: 1.43939, total profit: 0.07022, total orders: 188, median profit: 0.013665, median loss: -0.00122187500000000005
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
  
  // S7: Momentum Continuation (Trend Pullback Recovery)
  // Enter after a pullback within an established trend, confirmed by velocity recovery.
  // ADX confirms strong trend environment before attempting pullback entries.
  //median win-to-loss ratio: 0.598215, total profit: -0.01614, total orders: 63, median profit: -0.000555, median loss: -0.0009408928571428571
  val s7 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = 0, power = 2)
      ),
      Indicator.ValueTracking(
        role = ValueRole.Velocity,
        source = ValueSource.HLC3,
        transformation = ValueTransformation.KalmanVelocity(gain = 0.3, measurementNoise = 0.02)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.ADX(length = 14),
        upperBoundary = 50.0,
        lowerBoundary = 25.0
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 70.0,
        lowerBoundary = 30.0
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
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(4.hours),
            Rule.Condition.MomentumEntered(MomentumZone.Neutral), // recovering from oversold
            Rule.Condition.VelocityCrossedLevel(level = 0.0005, direction = Direction.Upward)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(4.hours),
            Rule.Condition.MomentumEntered(MomentumZone.Neutral), // recovering from overbought
            Rule.Condition.VelocityCrossedLevel(level = -0.0005, direction = Direction.Downward)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            // Velocity died
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.VelocityCrossedLevel(level = -0.0003, direction = Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.VelocityCrossedLevel(level = 0.0003, direction = Direction.Upward)
            ),
            // Time cap
            Rule.Condition.PositionOpenFor(8.hours),
            // Trend reversal
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

  //median win-to-loss ratio: 0.878305, total profit: 0.06374, total orders: 213, median profit: 0.014235, median loss: -0.00149102380952380985
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
        transformation = ValueTransformation.ADX(length = 13),
        upperBoundary = 50.0,
        lowerBoundary = 37.0
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
  
  // S8: Volatility Expansion Breakout
  // Trade the transition from low to high volatility, riding the initial impulse.
  // Uses Parabolic SAR as adaptive trailing exit instead of fixed velocity threshold.
  //median win-to-loss ratio: 0.549905, total profit: -0.13388, total orders: 291, median profit: -0.02077, median loss: -0.002378194444444444
  val s8 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 20),
        atrLength = 14,
        atrMultiplier = 2.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Velocity,
        source = ValueSource.HLC3,
        transformation = ValueTransformation.KalmanVelocity(gain = 0.3, measurementNoise = 0.02)
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 14,
        smoothingType = ValueTransformation.SMA(length = 20)
      ),
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 30, phase = 0, power = 2)
      ),
      // Parabolic SAR for adaptive trailing exit
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
            // Parabolic SAR flip — adaptive trailing stop
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.PriceCrossedLine(ValueRole.Momentum, Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.PriceCrossedLine(ValueRole.Momentum, Direction.Upward)
            ),
            // Trend reversal fallback
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

  //median win-to-loss ratio: 0.88141, total profit: 0.20505, total orders: 279, median profit: 0.034605, median loss: -0.0018013166666666669
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
  
  // S9: Dual-Timeframe Divergence
  // Fast CCI diverges from price trend — detect exhaustion and trade the reversal.
  // Uses Ichimoku Kijun-Sen as trend/equilibrium line and CCI for faster divergence detection.
  //median win-to-loss ratio: 1.61607, total profit: 0.00207, total orders: 139, median profit: 0.004975, median loss: -0.002370238095238095
  val s9 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // Trend via Ichimoku Kijun-Sen — price above = bullish, below = bearish
      Indicator.PriceLineCrossing(
        source = ValueSource.Close,
        role = ValueRole.ChannelMiddleBand,
        transformation = ValueTransformation.IchimokuKijunSen(length = 26)
      ),
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.IchimokuKijunSen(length = 26)
      ),
      // Fast CCI for divergence/reversal detection (unbounded, extreme readings are significant)
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.CCI(length = 14),
        upperBoundary = 100.0,
        lowerBoundary = -100.0
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 20),
        stdDevLength = 20,
        stdDevMultiplier = 2.0
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            // CCI was below -100 (oversold/pullback) and is now recovering
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            // Price touching/crossing lower band = deep pullback within uptrend
            Rule.Condition.LowerBandCrossed(Direction.Upward)
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            // CCI was above +100 (overbought/bounce) and is now falling
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            // Price touching/crossing upper band = bounce within downtrend
            Rule.Condition.UpperBandCrossed(Direction.Downward)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            // Take profit at Kijun-Sen (equilibrium line)
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Downward)
            ),
            // Trend reversal stop
            Rule.Condition.allOf(
              Rule.Condition.positionIsBuy,
              Rule.Condition.TrendChangedTo(Direction.Downward)
            ),
            Rule.Condition.allOf(
              Rule.Condition.positionIsSell,
              Rule.Condition.TrendChangedTo(Direction.Upward)
            ),
            // Time stop
            Rule.Condition.PositionOpenFor(6.hours)
          )
        )
      )
    )
  )

  //median win-to-loss ratio: 1.430485, total profit: 0.21718, total orders: 255, median profit: 0.043995, median loss: -0.0014979761904761906
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
  
  // S10: Fresh Momentum Continuation
  // Enter on fresh overbought/oversold in a confirmed trend — early momentum = continuation, not exhaustion.
  // ADX > 25 confirms we're in a trending environment where momentum continuation is valid.
  //median win-to-loss ratio: 0.64487, total profit: -0.01433, total orders: 367, median profit: -0.00193, median loss: -0.00078166025641025635
  val s10 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 45, phase = 0, power = 2)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 75.0,
        lowerBoundary = 25.0
      ),
      // ADX as trend strength confirmation
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.ADX(length = 14),
        upperBoundary = 50.0,
        lowerBoundary = 25.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 14,
        smoothingType = ValueTransformation.SMA(length = 20)
      ),
      Indicator.ValueTracking(
        role = ValueRole.Velocity,
        source = ValueSource.HLC3,
        transformation = ValueTransformation.KalmanVelocity(gain = 0.3, measurementNoise = 0.02)
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
            // Fresh entry into overbought = breakout continuation
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
            // Fresh entry into oversold = breakdown continuation
            Rule.Condition.momentumEnteredOversold,
            Rule.Condition.VelocityIs(Direction.Downward)
          )
        )
      ),
      closeRules = List(
        Rule(
          action = TradeAction.ClosePosition,
          conditions = Rule.Condition.anyOf(
            // Time-based exit — momentum edge decays
            Rule.Condition.PositionOpenFor(2.hours),
            // Momentum returned to neutral (edge gone)
            Rule.Condition.MomentumEntered(MomentumZone.Neutral),
            // Trend reversal
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

  //median win-to-loss ratio: 0.947155, total profit: 0.22800, total orders: 1845, median profit: 0.038195, median loss: -0.0006751422407488663
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
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.ADX(length = 8),
        upperBoundary = 61.0,
        lowerBoundary = 36.0
      ),
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
  //median win-to-loss ratio: 1.20441, total profit: 0.08671, total orders: 225, median profit: 0.010675, median loss: -0.00146237318840579735
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

  //median win-to-loss ratio: 1.319005, total profit: 0.06493, total orders: 215, median profit: 0.007155, median loss: -0.00156749999999999995
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
  // Trend + low-volatility filters screen out ranging markets. Parabolic SAR for adaptive trailing exit.
  // median win-to-loss ratio: 0.735755, total profit: 0.06177, total orders: 490, median profit: 0.01081, median loss: -0.0016911932367149762
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
      Indicator.PriceLineCrossing(
        source = ValueSource.Close,
        role = ValueRole.Momentum,
        transformation = ValueTransformation.ParabolicSAR(afStart = 0.04, afMax = 0.3, afStep = 0.04)
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
            // Parabolic SAR flip
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
            )
          )
        )
      )
    )
  )
}
