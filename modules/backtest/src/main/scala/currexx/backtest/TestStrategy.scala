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
  *
  * The thirteen vals from the 2026-08-25/26 rounds are numbered by descending validation fitness within each family rather than appended in
  * report order, and where two bases share one namespace — `s2_optimized`/`s2_optimized_v2`, `s12`/`s12_optimized` — that ordering runs
  * across both, so a suffix there says which champion the GA preferred and nothing about which is better.
  *
  * That batch is worth reading as one result rather than thirteen. Of the fourteen champions its sixteen rounds selected, three beat the
  * base they were optimised from on the holdout and six lose money there; nine had a training fitness of exactly 0.000000, meaning the six
  * search folds scored them at zero and the four-month validation fold ranked them alone. The ranking it produced is close to inverted: the
  * highest validation fitness of the batch and its only clean constraint verdict went to `s2_optimized_v7`, which returns a quarter of its
  * base's holdout net, while `s5_optimized_v3` — ninth by fitness, five constraints breached — posts the best holdout profit factor and
  * Sharpe in this file. Two further rounds selected nothing at all. Treat a validation figure from these rounds as a filter that rejects,
  * not as a ranking, and read the holdout column.
  */
object TestStrategy {

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-1855-s1_v2_optimized_shuffle.md (training 1.622058 -> validation 0.125887, retaining 7.8%, shuffled GA).
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.533, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - profit factor is 1.11609, required >= 1.2
  // Beats the s1_v2_optimized it came from on both corpora (1536 vs 1251 on the holdout) at a lower drawdown, despite the breaches above.
  // searched 2023-07..2025-07: net=6285.64646, closed=1259, forced=10, win=45.75%, exp=4.992571, PF=1.259, DD=1.79%, Sharpe=1.597
  // holdout 2025-12..2026-06:  net=1535.84151, closed=382, forced=6, win=45.55%, exp=4.020528, PF=1.242, DD=0.85%, Sharpe=1.548
  val s1_v2_optimized = TestStrategy(
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

  // GA-optimized indicator params for s5_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-25-2011-s5_optimized.md (training 0.387064 -> validation 0.106525, retaining 27.5%).
  // The only s5 champion of the 2026-08-25/26 batch: the shuffled twin
  // (ga-optimisation-2026-08-26-0747-s5_optimized_shuffle.md) had no finalist score above zero on validation and selected nothing.
  // BREACHES 5 constraint(s) on validation data:
  //   - closed trades is 97, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - profitable pair-months is 0.519, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - pair-month profit factor is 1.229396636418566559057141030972838, required >= 1.3
  //   - profit factor is 1.13760, required >= 1.2
  // The best champion this batch produced, and on the holdout the best profit factor (2.079) and Sharpe (4.131) in the catalogue, at a
  // 0.49% drawdown. Beats the s5_optimized it came from on both corpora — 4222 vs 3089 in sample, 1607 vs 705 out — and its holdout PF is
  // higher than its in-sample PF, so the improvement is not a fit to the folds.
  // Its GA fitness said none of this: 0.106525 on validation with five breached constraints, ninth of the fourteen champions measured. The
  // same lesson s5_optimized_v2 taught in the previous batch, from the same base.
  // searched 2023-07..2025-07: net=4221.68925, closed=528, forced=4, win=67.05%, exp=7.995624, PF=1.721, DD=0.35%, Sharpe=3.063
  // holdout 2025-12..2026-06:  net=1607.43953, closed=149, forced=0, win=70.47%, exp=10.788185, PF=2.079, DD=0.49%, Sharpe=4.131
  val s5_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = -6, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 41,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 28,
        smoothingType = ValueTransformation.SMA(length = 63)
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
        transformation = ValueTransformation.RSX(length = 8)
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
}
