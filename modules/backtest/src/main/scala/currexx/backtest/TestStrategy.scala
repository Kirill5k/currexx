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
  * Every val carries two metrics lines because one number cannot say whether a strategy works. `searched 2023-07..2025-07` is the two years
  * the GA folds cover, so for anything named `_optimized` it reports fit to the data that chose it and is not evidence of an edge.
  * `holdout 2025-12..2026-06` is the seven months nothing has ever scored or selected against; that is the line to read. Where the two
  * disagree sharply the strategy is fitted, not skilled — the clearest case here is s4_regime_optimized, PF 1.894 in-sample against 0.595
  * out, on a corpus where its whole family loses money.
  *
  * Read the holdout column across strategies, not as a forecast. Its net figures cover seven months against the searched column's
  * twenty-four, so they are not comparable to each other, and eighteen of twenty-one vals post a lower profit factor there than in sample —
  * the usual overfitting story, but it also means the level of the whole column is a property of those seven months. What it supports is
  * ranking strategies against each other on data none of them was selected on. What it does not support is expecting those numbers forward.
  *
  * Both lines were re-measured on 2026-08-25 when the folds went from three to six and reporting moved to the holdout. They are not
  * comparable to figures quoted in commit history or in `ga-optimisation-*.md` reports before that date: the earlier "searched" column was
  * one year rather than two, and the earlier out-of-sample column was the whole newer export, a third of which is the validation fold every
  * champion's finalist ranking selected on.
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
  // searched 2023-07..2025-07: net=5716.99531, closed=1291, forced=12, win=48.03%, exp=4.428346, PF=1.242, DD=1.40%, Sharpe=1.460
  // holdout 2025-12..2026-06:  net=1250.85597, closed=364, forced=6, win=46.43%, exp=3.436417, PF=1.194, DD=1.23%, Sharpe=1.107
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
  // Beats the s1_v2_optimized it came from on both corpora (1536 vs 1251 on the holdout) at a lower drawdown, despite the breaches above.
  // searched 2023-07..2025-07: net=6285.64646, closed=1259, forced=10, win=45.75%, exp=4.992571, PF=1.259, DD=1.79%, Sharpe=1.597
  // holdout 2025-12..2026-06:  net=1535.84151, closed=382, forced=6, win=45.55%, exp=4.020528, PF=1.242, DD=0.85%, Sharpe=1.548
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
  // searched 2023-07..2025-07: net=5894.88045, closed=1124, forced=6, win=69.84%, exp=5.244556, PF=1.362, DD=1.13%, Sharpe=1.774
  // holdout 2025-12..2026-06:  net=796.16752, closed=319, forced=2, win=70.22%, exp=2.495823, PF=1.156, DD=1.34%, Sharpe=1.017
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
  // searched 2023-07..2025-07: net=5825.50423, closed=1218, forced=12, win=38.01%, exp=4.782844, PF=1.254, DD=2.43%, Sharpe=1.304
  // holdout 2025-12..2026-06:  net=2480.22278, closed=352, forced=5, win=40.63%, exp=7.046087, PF=1.434, DD=1.74%, Sharpe=2.142
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
  // Satisfies every constraint on validation data — the only champion of the 2026-08-24/25 batch that does.
  // Best holdout net and Sharpe in the catalogue: beats s2_optimized on data neither was selected on (2660 vs 2480, Sharpe 2.628 vs 2.142)
  // while giving up in-sample net to it (4923 vs 5826). The margin on net is 7%, thin enough that the Sharpe gap is the better reason to
  // prefer it.
  // searched 2023-07..2025-07: net=4922.55728, closed=1203, forced=12, win=37.82%, exp=4.091901, PF=1.219, DD=1.98%, Sharpe=1.183
  // holdout 2025-12..2026-06:  net=2660.48001, closed=363, forced=5, win=42.98%, exp=7.329146, PF=1.458, DD=1.70%, Sharpe=2.628
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
  // Trades far more than s2_optimized (1938 vs 1218 in sample) for less than half its holdout net (1123 vs 2480) — the extra volume is
  // mostly cost: 1913 of 7311 gross in sample.
  // searched 2023-07..2025-07: net=5398.00328, closed=1938, forced=11, win=42.16%, exp=2.785347, PF=1.190, DD=2.51%, Sharpe=1.297
  // holdout 2025-12..2026-06:  net=1122.85657, closed=551, forced=5, win=42.29%, exp=2.037852, PF=1.139, DD=2.06%, Sharpe=1.113
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
  // Beats its s2_optimized_v2 base on the holdout (1016 vs 796) and loses to it in sample, on 1892 trades against 1124 and at the worst
  // holdout drawdown in the catalogue, 2.90%.
  // searched 2023-07..2025-07: net=5615.41274, closed=1892, forced=12, win=41.49%, exp=2.967977, PF=1.197, DD=2.07%, Sharpe=1.361
  // holdout 2025-12..2026-06:  net=1016.36311, closed=561, forced=5, win=41.36%, exp=1.811699, PF=1.115, DD=2.90%, Sharpe=1.020
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
  // Highest in-sample net of the whole s2 family (6308) and below its own s2_optimized_v2 base on the holdout (738 vs 796): the shape the
  // 1.2% retention predicted, and the earlier contaminated measurement had it the other way round.
  // searched 2023-07..2025-07: net=6307.72496, closed=1057, forced=6, win=70.29%, exp=5.967573, PF=1.417, DD=1.33%, Sharpe=2.070
  // holdout 2025-12..2026-06:  net=738.35880, closed=289, forced=2, win=71.28%, exp=2.554875, PF=1.164, DD=1.12%, Sharpe=0.975
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

  // Baseline kept for lineage — the three vals below are GA descendants of it. Break-even at best on the holdout (net 45, PF 1.020), so it
  // is not in BatchBacktester.
  // searched 2023-07..2025-07: net=3117.14616, closed=523, forced=3, win=68.64%, exp=5.960127, PF=1.463, DD=0.71%, Sharpe=1.488
  // holdout 2025-12..2026-06:  net=44.87172, closed=154, forced=1, win=66.88%, exp=0.291375, PF=1.020, DD=0.97%, Sharpe=0.104
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
  // searched 2023-07..2025-07: net=3435.55301, closed=467, forced=4, win=69.59%, exp=7.356645, PF=1.621, DD=0.54%, Sharpe=1.984
  // holdout 2025-12..2026-06:  net=622.99662, closed=141, forced=1, win=73.76%, exp=4.418416, PF=1.363, DD=0.49%, Sharpe=1.771
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
  // Not in BatchBacktester. On the clean holdout s4_optimized_v2 out-earns it (623 vs 285) on a third of the trades, reversing the earlier
  // contaminated reading that had this one ahead.
  // searched 2023-07..2025-07: net=936.96896, closed=953, forced=2, win=51.42%, exp=0.983178, PF=1.082, DD=1.37%, Sharpe=0.353
  // holdout 2025-12..2026-06:  net=284.71195, closed=298, forced=0, win=50.67%, exp=0.955409, PF=1.078, DD=1.96%, Sharpe=0.325
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
  // The most interesting result of the batch. It gives up most of its in-sample net against s4_optimized_v2 (1281 vs 3436) and beats it on
  // the holdout (1080 vs 623), where it posts the best profit factor in the catalogue (1.565) and the second-best Sharpe (2.189) at a 0.72%
  // drawdown.
  // One of only three vals whose profit factor is higher on the holdout than in sample, and by far the widest gap of them (1.155 -> 1.565):
  // whatever the GA found here, it was not a fit to the folds.
  // searched 2023-07..2025-07: net=1281.32473, closed=788, forced=2, win=57.11%, exp=1.626047, PF=1.155, DD=1.25%, Sharpe=0.669
  // holdout 2025-12..2026-06:  net=1079.89722, closed=210, forced=0, win=61.43%, exp=5.142368, PF=1.565, DD=0.72%, Sharpe=2.189
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
  // The sharpest overfit in the catalogue: PF 1.894 on the corpus that chose it collapses to 0.595 on the holdout, the worst there.
  // searched 2023-07..2025-07: net=2657.82001, closed=302, forced=1, win=67.55%, exp=8.800729, PF=1.894, DD=0.46%, Sharpe=1.805
  // holdout 2025-12..2026-06:  net=-519.55814, closed=83, forced=0, win=51.81%, exp=-6.259737, PF=0.595, DD=1.39%, Sharpe=-1.640
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
  // searched 2023-07..2025-07: net=1435.12725, closed=534, forced=0, win=53.93%, exp=2.687504, PF=1.225, DD=1.29%, Sharpe=0.765
  // holdout 2025-12..2026-06:  net=-224.33386, closed=163, forced=0, win=43.56%, exp=-1.376281, PF=0.887, DD=1.35%, Sharpe=-0.519
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
  // Loses money on the holdout (-329), as does the s4_regime_optimized_v2 it came from (-224) and the s4_regime_optimized both descend from
  // (-520). The earlier contaminated measurement showed all three as marginally positive; on data nothing selected on, the whole
  // regime-gated line is unprofitable and none of it belongs in BatchBacktester on merit.
  // searched 2023-07..2025-07: net=1405.00047, closed=560, forced=0, win=56.43%, exp=2.508929, PF=1.214, DD=0.90%, Sharpe=0.891
  // holdout 2025-12..2026-06:  net=-328.61663, closed=171, forced=0, win=46.78%, exp=-1.921735, PF=0.846, DD=1.45%, Sharpe=-0.662
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
  // Named for replacing an earlier val also called s5_optimized_v2, which was pruned; the s5_optimized_v2 now below it is a different
  // strategy, a 2026-08-24 champion descended from this one.
  // searched 2023-07..2025-07: net=3088.80592, closed=646, forced=4, win=65.79%, exp=4.781433, PF=1.376, DD=0.38%, Sharpe=2.021
  // holdout 2025-12..2026-06:  net=704.90592, closed=176, forced=0, win=67.05%, exp=4.005147, PF=1.298, DD=1.08%, Sharpe=1.442
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
  // Beats the s5_optimized it came from on both corpora — 4189 vs 3089 in sample, 857 vs 705 on the holdout, at a better PF and Sharpe on
  // each. Its GA fitness said the opposite: 0.0% retention, five breached constraints, selected only because its validation score rounds to
  // zero from above. The cleanest example in this catalogue of the validation fold being too small a sample to trust on its own.
  // searched 2023-07..2025-07: net=4188.72153, closed=666, forced=6, win=67.72%, exp=6.289372, PF=1.514, DD=0.62%, Sharpe=2.175
  // holdout 2025-12..2026-06:  net=856.74552, closed=180, forced=0, win=67.22%, exp=4.759697, PF=1.376, DD=0.83%, Sharpe=2.080
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
  // Kept for structural coverage: this is the only strategy in the catalogue that reads volume. It is flat to slightly negative on the
  // holdout (net -79, PF 0.976) — the best result of any s12 val, none of which makes money there. The 2026-08-25 rounds gave it the fresh
  // optimisation its transformation-aware threshold bounds called for; the answer was that s12 is searchable but not profitable.
  // searched 2023-07..2025-07: net=3291.74961, closed=309, forced=9, win=48.87%, exp=10.652911, PF=1.326, DD=0.99%, Sharpe=1.225
  // holdout 2025-12..2026-06:  net=-78.88478, closed=80, forced=3, win=52.50%, exp=-0.986060, PF=0.976, DD=1.64%, Sharpe=-0.115
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
  // searched 2023-07..2025-07: net=4388.13422, closed=338, forced=10, win=52.37%, exp=12.982646, PF=1.400, DD=0.87%, Sharpe=1.646
  // holdout 2025-12..2026-06:  net=-766.55748, closed=93, forced=4, win=46.24%, exp=-8.242554, PF=0.809, DD=1.88%, Sharpe=-1.197
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
  // Loses more on the holdout than the s12 it came from (-291 vs -79) despite a higher in-sample net. The contaminated measurement had this
  // as a large improvement on s12; on clean data it is a regression.
  // searched 2023-07..2025-07: net=1916.93620, closed=339, forced=6, win=42.77%, exp=5.654679, PF=1.214, DD=2.11%, Sharpe=0.597
  // holdout 2025-12..2026-06:  net=-291.10970, closed=100, forced=1, win=46.00%, exp=-2.911097, PF=0.893, DD=0.84%, Sharpe=-0.574
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
  // Barely improves on the s12_optimized it came from (-736 vs -767 on the holdout) for more than double the trades. The worst holdout
  // profit factor of the s12 family bar s12_optimized itself, and like all of it, loss-making.
  // searched 2023-07..2025-07: net=1351.81190, closed=682, forced=3, win=42.38%, exp=1.982129, PF=1.110, DD=1.68%, Sharpe=0.498
  // holdout 2025-12..2026-06:  net=-736.12992, closed=198, forced=0, win=42.42%, exp=-3.717828, PF=0.826, DD=2.19%, Sharpe=-1.020
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
