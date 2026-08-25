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

/** The strategies worth measuring, and what each one actually scored.
  *
  * Every val carries two metrics lines because one number cannot say whether a strategy works. `majors1h` is the corpus the GA searched
  * against, so for anything named `_optimized` it reports fit to the data that chose it and is not evidence of an edge.
  * `majors1h_202507_202606` is the later export, which no search scored against; that is the line to read. Where the two disagree sharply
  * the strategy is fitted, not skilled — the clearest case here is s4_regime_optimized, PF 4.013 in-sample against 0.623 out.
  *
  * Version suffixes are contiguous within each family but no longer rank it. They were renumbered by out-of-sample net when the catalogue
  * was pruned, and the 2026-08-24/25 champions were then appended to the next free number in each family, so the ordering holds only among
  * the vals that predate them — `s2_optimized_v3` has the best out-of-sample net here despite its suffix. A champion's version need not
  * match the label in the `ga-optimisation-*.md` report it came from either; the report filename in each comment is the stable link back.
  */
object TestStrategy {

  // GA-optimized indicator params for s1_v2_optimized_v2, which is no longer in this catalogue (rules unchanged). Champion from
  // ga-optimisation-2026-08-08-1409-s1_v2_optimized_v2_shuffle.md (training 1.157784 -> validation 0.053450, shuffled GA).
  // BREACHES 4 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - pair-month profit factor is 1.239396792464384156770483855333763, required >= 1.3
  //   - profit factor is 1.09198, required >= 1.2
  // majors1h (searched):     net=6648.19399, closed=696, forced=6, win=48.56%, exp=9.552003, PF=1.619, DD=1.14%, Sharpe=3.872
  // majors1h_202507_202606:  net=1915.81314, closed=640, forced=6, win=44.22%, exp=2.993458, PF=1.168, DD=1.45%, Sharpe=1.118
  val s1_v2_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // Primary signal: JMA crossover
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 32, phase = 11, power = 5),
        line2Transformation = ValueTransformation.JMA(length = 29, phase = 1, power = 2)
      ),
      // Momentum filter
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 22),
        upperBoundary = 69.0,
        lowerBoundary = 26.0
      ),
      // Momentum tracking
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 15)
      ),
      // Volatility filter
      Indicator.VolatilityRegimeDetection(
        atrLength = 27,
        smoothingType = ValueTransformation.SMA(length = 35)
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-1855-s1_v2_optimized_shuffle.md (training 1.622058 -> validation 0.125887, retaining 7.8%, shuffled GA).
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.533, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - profit factor is 1.11609, required >= 1.2
  // Beats the s1_v2_optimized it came from on both halves (2385 vs 1916 out of sample) at a lower drawdown, despite the breaches above.
  // majors1h (searched):     net=7188.03682, closed=639, forced=4, win=47.57%, exp=11.248884, PF=1.669, DD=0.85%, Sharpe=3.665
  // majors1h_202507_202606:  net=2384.66472, closed=643, forced=6, win=42.92%, exp=3.708654, PF=1.218, DD=1.16%, Sharpe=1.576
  val s1_v2_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // Primary signal: JMA crossover
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 19, phase = 21, power = 3),
        line2Transformation = ValueTransformation.JMA(length = 42, phase = 1, power = 3)
      ),
      // Momentum filter
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 27),
        upperBoundary = 71.0,
        lowerBoundary = 30.0
      ),
      // Momentum tracking
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 15)
      ),
      // Volatility filter
      Indicator.VolatilityRegimeDetection(
        atrLength = 24,
        smoothingType = ValueTransformation.SMA(length = 36)
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

  // GA-optimized indicator params for s2, which is no longer in this catalogue (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1755-s2.md (training 2.047228 -> validation 0.063279).
  // BREACHES 3 constraint(s) on validation data:
  //   - pair-month profit factor is 1.224584358662623739024413542690434, required >= 1.3
  //   - profit factor is 1.08474, required >= 1.2
  //   - profitable datasets is 0.333, required >= 0.667
  // majors1h (searched):     net=4882.14619, closed=602, forced=3, win=71.26%, exp=8.109877, PF=1.623, DD=1.65%, Sharpe=2.765
  // majors1h_202507_202606:  net=979.82518, closed=527, forced=2, win=69.26%, exp=1.859251, PF=1.118, DD=1.76%, Sharpe=0.581
  val s2_optimized_v2 = TestStrategy(
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

  // GA-optimized indicator params for s2, which is no longer in this catalogue (rules unchanged). Champion from
  // ga-optimisation-2026-08-08-1436-s2_shuffle.md (training 1.499322 -> validation 0.973937, shuffled GA).
  // Satisfies every constraint on validation data. Second-best out-of-sample result in the catalogue, behind its own descendant
  // s2_optimized_v3.
  // majors1h (searched):     net=7644.55357, closed=638, forced=6, win=40.13%, exp=11.982059, PF=1.727, DD=1.31%, Sharpe=4.780
  // majors1h_202507_202606:  net=3386.47604, closed=602, forced=5, win=38.70%, exp=5.625375, PF=1.335, DD=1.71%, Sharpe=1.722
  val s2_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 19, phase = 14, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 32, phase = -43, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 74.0,
        lowerBoundary = 29.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 37,
        smoothingType = ValueTransformation.SMA(length = 35)
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

  // GA-optimized indicator params for s2_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-1811-s2_optimized.md (training 1.430220 -> validation 1.043568, retaining 73.0%).
  // Satisfies every constraint on validation data. Highest validation retention of the 2026-08-24 batch.
  // Best out-of-sample net in the catalogue: beats s2_optimized on the later year (3797 vs 3386) while giving up a little in sample.
  // majors1h (searched):     net=6628.82642, closed=607, forced=6, win=40.53%, exp=10.920637, PF=1.666, DD=1.21%, Sharpe=4.922
  // majors1h_202507_202606:  net=3797.28868, closed=613, forced=5, win=40.78%, exp=6.194598, PF=1.372, DD=1.67%, Sharpe=2.130
  val s2_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 20, phase = 21, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 31, phase = -40, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 31),
        upperBoundary = 72.0,
        lowerBoundary = 27.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 37,
        smoothingType = ValueTransformation.SMA(length = 35)
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

  // GA-optimized indicator params for s2_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-1827-s2_optimized_shuffle.md (training 1.352316 -> validation 0.312456, retaining 23.1%, shuffled GA).
  // BREACHES 2 constraint(s) on validation data:
  //   - profitable pair-months is 0.533, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  // Trades far more than s2_optimized (976 vs 638 in sample) for less out-of-sample net (2024 vs 3386) — the extra volume is mostly cost.
  // majors1h (searched):     net=6888.05607, closed=976, forced=5, win=45.29%, exp=7.057434, PF=1.542, DD=1.38%, Sharpe=4.463
  // majors1h_202507_202606:  net=2023.52080, closed=926, forced=5, win=42.44%, exp=2.185228, PF=1.156, DD=2.03%, Sharpe=1.241
  val s2_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 22, phase = 47, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 29, phase = 1, power = 2)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 19),
        upperBoundary = 70.0,
        lowerBoundary = 25.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 37,
        smoothingType = ValueTransformation.SMA(length = 35)
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
  // ga-optimisation-2026-08-24-1948-s2_optimized_v2_shuffle.md (training 1.358189 -> validation 0.311945, retaining 23.0%, shuffled GA).
  // BREACHES 2 constraint(s) on validation data:
  //   - profitable pair-months is 0.433, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  // Beats its s2_optimized_v2 base on both halves (1455 vs 980 out of sample) but on 930 trades against 527, at the worst drawdown here.
  // majors1h (searched):     net=7186.34965, closed=960, forced=6, win=43.75%, exp=7.485781, PF=1.565, DD=1.23%, Sharpe=4.222
  // majors1h_202507_202606:  net=1455.40231, closed=930, forced=5, win=40.86%, exp=1.564949, PF=1.101, DD=2.88%, Sharpe=0.904
  val s2_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 28, phase = 41, power = 3),
        line2Transformation = ValueTransformation.JMA(length = 33, phase = -14, power = 2)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 20),
        upperBoundary = 72.0,
        lowerBoundary = 24.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 41,
        smoothingType = ValueTransformation.SMA(length = 29)
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
  // ga-optimisation-2026-08-24-1910-s2_optimized_v2.md (training 0.758049 -> validation 0.008849, retaining 1.2%).
  // Retained almost nothing of its training score, so the search found nothing here that survives outside its own sample.
  // BREACHES 3 constraint(s) on validation data:
  //   - pair-month profit factor is 1.080926847720823257065371121911642, required >= 1.3
  //   - profit factor is 1.05023, required >= 1.2
  //   - costs as a share of gross profit is 0.567, required <= 0.400
  // Measured better than its s2_optimized_v2 base out of sample anyway (1242 vs 980) on fewer trades, which the 1.2% retention did not
  // predict — one reading of one later year, not evidence the search worked.
  // majors1h (searched):     net=4670.04922, closed=575, forced=3, win=72.70%, exp=8.121825, PF=1.619, DD=1.94%, Sharpe=3.077
  // majors1h_202507_202606:  net=1242.13579, closed=478, forced=2, win=70.50%, exp=2.598610, PF=1.169, DD=1.41%, Sharpe=0.801
  val s2_optimized_v6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 38, phase = -39, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 24, phase = 13, power = 6)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 18),
        upperBoundary = 58.0,
        lowerBoundary = 9.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 7,
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

  // Baseline kept for lineage — the two vals below are GA descendants of it. Loses money out of sample, so it is not in BatchBacktester.
  // majors1h (searched):     net=3749.11726, closed=249, forced=3, win=76.31%, exp=15.056696, PF=2.642, DD=0.59%, Sharpe=3.718
  // majors1h_202507_202606:  net=-304.99815, closed=266, forced=1, win=66.54%, exp=-1.146610, PF=0.925, DD=1.30%, Sharpe=-0.308
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
  // majors1h (searched):     net=3649.62245, closed=225, forced=3, win=76.44%, exp=16.220544, PF=2.899, DD=0.46%, Sharpe=4.488
  // majors1h_202507_202606:  net=300.43917, closed=244, forced=1, win=69.26%, exp=1.231308, PF=1.092, DD=1.32%, Sharpe=0.309
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
  // Not in BatchBacktester, though it out-earns s4_optimized_v2 out of sample (+486 vs +300) on nearly double the trades.
  // majors1h (searched):     net=2215.86261, closed=485, forced=2, win=54.43%, exp=4.568789, PF=1.428, DD=1.26%, Sharpe=1.743
  // majors1h_202507_202606:  net=486.01765, closed=497, forced=0, win=51.91%, exp=0.977903, PF=1.081, DD=1.95%, Sharpe=0.363
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

  // GA-optimized indicator params for s4_optimized_v2 (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-2107-s4_optimized_v2_shuffle.md (training 1.057177 -> validation 0.485363, retaining 45.9%, shuffled GA).
  // BREACHES 1 constraint(s) on validation data:
  //   - most concentrated pair's best month is 0.776, required <= 0.755 (0.700 scaled to 4 periods)
  // Gives up in-sample net against s4_optimized_v2 (2596 vs 3650) and quadruples it out of sample (1255 vs 300) on 385 trades against 244.
  // majors1h (searched):     net=2595.56088, closed=391, forced=1, win=62.40%, exp=6.638263, PF=1.759, DD=0.56%, Sharpe=2.917
  // majors1h_202507_202606:  net=1254.60741, closed=385, forced=0, win=61.56%, exp=3.258721, PF=1.332, DD=0.80%, Sharpe=1.433
  val s4_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 27, phase = -19, power = 1)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 26),
        atrLength = 19,
        atrMultiplier = 2.3
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 11),
        upperBoundary = 65.0,
        lowerBoundary = 25.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 23,
        smoothingType = ValueTransformation.SMA(length = 49)
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

  // Baseline kept for lineage — s4_regime_optimized_v2 below is its GA descendant. Not in BatchBacktester.
  // The sharpest overfit in the catalogue: PF 4.013 on the corpus that chose it collapses to 0.623 on the later year.
  // majors1h (searched):     net=2924.93376, closed=165, forced=1, win=77.58%, exp=17.726871, PF=4.013, DD=0.21%, Sharpe=3.856
  // majors1h_202507_202606:  net=-1018.22412, closed=166, forced=0, win=56.63%, exp=-6.133880, PF=0.623, DD=2.05%, Sharpe=-1.272
  val s4_regime_optimized = TestStrategy(
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

  // GA-optimized indicator params for s4_regime_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-08-1546-s4_regime_optimized_v2.md (training 0.773123 -> validation 0.687602).
  // BREACHES 2 constraint(s) on validation data:
  //   - closed trades is 103, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - most concentrated pair's best month is 0.756, required <= 0.755 (0.700 scaled to 4 periods)
  // majors1h (searched):     net=2312.35834, closed=269, forced=0, win=62.45%, exp=8.596128, PF=1.834, DD=0.42%, Sharpe=2.769
  // majors1h_202507_202606:  net=156.64307, closed=289, forced=0, win=50.52%, exp=0.542018, PF=1.049, DD=1.34%, Sharpe=0.232
  val s4_regime_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = -26, power = 2)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 31),
        atrLength = 26,
        atrMultiplier = 1.6
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 22),
        upperBoundary = 67.0,
        lowerBoundary = 35.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 25,
        smoothingType = ValueTransformation.SMA(length = 54)
      ),
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

  // GA-optimized indicator params for s4_regime_optimized_v2 (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-2125-s4_regime_optimized_v2.md (training 0.542873 -> validation 0.671006, retaining 123.6%).
  // The only champion of the 2026-08-24 batch to score higher on validation than on the folds it was searched against.
  // BREACHES 2 constraint(s) on validation data:
  //   - closed trades is 111, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - most concentrated pair's best month is 0.938, required <= 0.755 (0.700 scaled to 4 periods)
  // Roughly doubles its s4_regime_optimized_v2 base out of sample (295 vs 157) while giving up a fifth in sample. Both are near break-even.
  // majors1h (searched):     net=1842.48903, closed=288, forced=0, win=64.24%, exp=6.397531, PF=1.600, DD=0.39%, Sharpe=2.470
  // majors1h_202507_202606:  net=294.95889, closed=302, forced=0, win=53.31%, exp=0.976685, PF=1.091, DD=1.43%, Sharpe=0.394
  val s4_regime_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = -19, power = 2)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 28),
        atrLength = 31,
        atrMultiplier = 1.6
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 23),
        upperBoundary = 66.0,
        lowerBoundary = 34.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 22,
        smoothingType = ValueTransformation.SMA(length = 55)
      ),
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

  // GA-optimized indicator params for s5_optimized_v2, which is no longer in this catalogue (rules unchanged). Champion from
  // ga-optimisation-2026-08-08-1634-s5_optimized_v2.md (training 0.660687 -> validation 0.038808).
  // BREACHES 4 constraint(s) on validation data:
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - pair-month profit factor is 1.223093031403288088998350627052696, required >= 1.3
  //   - profit factor is 1.07928, required >= 1.2
  //   - costs as a share of gross profit is 0.481, required <= 0.400
  // Gives up a little in-sample against the s5_optimized_v2 it replaced (2614 vs 2705) and nearly triples it out of sample (705 vs 247).
  // majors1h (searched):     net=2614.14803, closed=316, forced=2, win=69.62%, exp=8.272620, PF=1.774, DD=0.52%, Sharpe=3.024
  // majors1h_202507_202606:  net=705.14380, closed=318, forced=0, win=66.67%, exp=2.217433, PF=1.161, DD=1.08%, Sharpe=1.084
  val s5_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 47, phase = 23, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 49),
        stdDevLength = 39,
        stdDevMultiplier = 2.7
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 27,
        smoothingType = ValueTransformation.SMA(length = 49)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 66.0,
        lowerBoundary = 31.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 9)
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

  // GA-optimized indicator params for s5_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-2000-s5_optimized.md (training 0.963309 -> validation 0.000000, retaining 0.0%).
  // Selected only because its validation score rounds to zero from above rather than being zero; the round found nothing that holds up
  // outside its own folds, and every consistency constraint fails.
  // BREACHES 5 constraint(s) on validation data:
  //   - profitable pair-months is 0.440, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - pair-month profit factor is 1.002557000513026028408320455552660, required >= 1.3
  //   - profit factor is 1.00098, required >= 1.2
  //   - costs as a share of gross profit is 0.986, required <= 0.400
  // Gains 30% in sample over s5_optimized (3411 vs 2614) and loses out of sample (670 vs 705) — a fitted improvement, as the 0.0% retention
  // said it would be.
  // majors1h (searched):     net=3410.54531, closed=322, forced=2, win=72.98%, exp=10.591756, PF=2.036, DD=0.63%, Sharpe=3.362
  // majors1h_202507_202606:  net=669.73885, closed=325, forced=0, win=64.31%, exp=2.060735, PF=1.149, DD=1.18%, Sharpe=0.908
  val s5_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 42, phase = -7, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 49),
        stdDevLength = 39,
        stdDevMultiplier = 2.7
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 24,
        smoothingType = ValueTransformation.SMA(length = 49)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 11),
        upperBoundary = 66.0,
        lowerBoundary = 30.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6)
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
  // The W/L and total-profit figures above predate the current cost and risk model and are not comparable to the metrics below.
  //
  // Kept for structural coverage: this is the only strategy in the catalogue that reads volume. Its historical out-of-sample result is
  // negative, while the four 2026-08-08 optimisation reports used the old percentage-only CMF threshold search. Threshold bounds are now
  // transformation-aware, so s12 needs a fresh optimisation before its current searchability can be assessed.
  // majors1h (searched):     net=2885.12973, closed=152, forced=5, win=51.97%, exp=18.981117, PF=1.534, DD=1.95%, Sharpe=2.045
  // majors1h_202507_202606:  net=-1413.68208, closed=129, forced=3, win=48.06%, exp=-10.958776, PF=0.750, DD=3.13%, Sharpe=-1.187
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
  // ga-optimisation-2026-07-05-1349-s12.md (fitness 0.407640, single-score format predating the training/validation split).
  // Kept alongside s12 for volume coverage; like s12 it is unprofitable out of sample.
  // majors1h (searched):     net=3800.76555, closed=168, forced=5, win=54.17%, exp=22.623604, PF=1.702, DD=1.68%, Sharpe=2.726
  // majors1h_202507_202606:  net=-1918.87451, closed=155, forced=4, win=43.23%, exp=-12.379836, PF=0.714, DD=3.69%, Sharpe=-1.595
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

  // GA-optimized indicator params for s12 (rules unchanged). Champion from
  // ga-optimisation-2026-08-25-0148-s12_shuffle.md (training 0.451368 -> validation 0.101959, retaining 22.6%, shuffled GA).
  // First s12 champion found since threshold bounds became transformation-aware, so this is the first honest read on s12's searchability.
  // BREACHES 4 constraint(s) on validation data:
  //   - closed trades is 70, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - profitable pair-months is 0.536, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - profit factor is 1.19757, required >= 1.2
  // Still loses money out of sample, but cuts s12's loss by more than two thirds (-412 vs -1414) and is the best in-sample PF of the s12
  // family at 2.176. The transformation-aware threshold search helped; it did not make s12 profitable.
  // majors1h (searched):     net=3921.03444, closed=153, forced=5, win=52.29%, exp=25.627676, PF=2.176, DD=1.24%, Sharpe=2.447
  // majors1h_202507_202606:  net=-412.41631, closed=180, forced=1, win=47.22%, exp=-2.291202, PF=0.913, DD=1.55%, Sharpe=-0.398
  val s12_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.IchimokuKijunSen(length = 23)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.CMF(length = 18),
        upperBoundary = 0.28,
        lowerBoundary = -0.18
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 32,
        smoothingType = ValueTransformation.SMA(length = 42)
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
            Rule.Condition.momentumEnteredOverbought, // CMF crossed above +0.28
            Rule.Condition.volatilityIsLow
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.momentumEnteredOversold, // CMF crossed below -0.18
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

  // GA-optimized indicator params for s12_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-25-0808-s12_optimized_shuffle.md (training 0.682651 -> validation 0.000044, retaining 0.0%, shuffled GA).
  // Selected only because its validation score is non-zero rather than zero; the round found nothing that holds up outside its own folds.
  // BREACHES 6 constraint(s) on validation data:
  //   - profitable pair-months is 0.444, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - pair-month profit factor is 1.021028615269642604539967629157884, required >= 1.3
  //   - profit factor is 1.01217, required >= 1.2
  //   - costs as a share of gross profit is 0.851, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Cuts s12_optimized's out-of-sample loss from -1919 to -406, on more than double the trades. Still loss-making, like everything in the
  // s12 family.
  // majors1h (searched):     net=3059.17812, closed=325, forced=2, win=44.31%, exp=9.412856, PF=1.534, DD=1.42%, Sharpe=2.030
  // majors1h_202507_202606:  net=-406.33636, closed=352, forced=0, win=42.05%, exp=-1.154365, PF=0.936, DD=2.34%, Sharpe=-0.339
  val s12_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.IchimokuKijunSen(length = 13)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.CMF(length = 13),
        upperBoundary = 0.26,
        lowerBoundary = -0.24
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 26,
        smoothingType = ValueTransformation.SMA(length = 11)
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
            Rule.Condition.momentumEnteredOverbought, // CMF crossed above +0.26
            Rule.Condition.volatilityIsLow
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(1.hour),
            Rule.Condition.momentumEnteredOversold, // CMF crossed below -0.24
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
