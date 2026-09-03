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
  * out, on a corpus where its whole family loses money. `BatchBacktester` prints a third section for the original twelve-month sample; it
  * is a subset of `searched` and is deliberately not recorded per val.
  *
  * Read the holdout column across strategies, not as a forecast. Its net figures cover seven months against the searched column's
  * twenty-four, so they are not comparable to each other, and nine of the thirteen vals post a lower profit factor there than in sample —
  * the usual overfitting story, but it also means the level of the whole column is a property of those seven months. What it supports is
  * ranking strategies against each other on data none of them was selected on. What it does not support is expecting those numbers forward.
  *
  * The four exceptions, whose holdout profit factor beats their in-sample one, are s2_optimized, s2_optimized_v3, s4_optimized_v2 and
  * s5_optimized_v2 — and three of those four lead the catalogue on holdout net. That is the pattern worth trusting: a strategy holding up
  * better on data nobody selected it on than on the data that chose it was not fitted to the folds.
  *
  * The searched column hides a split worth knowing about, found on 2026-08-31 by scoring its two exports separately. Four vals here make
  * money in 2023-07..2024-06 - s6 (+3844), s5_optimized_v2 (+1406), s2_optimized_v2 (+1013) and s12_optimized (+587), with s12 (+407) -
  * while every JMA-crossover val loses there: s2_optimized returns -1819 against +7645 in 2024-07..2025-07, s2_optimized_v3 -1706 against
  * +6629, s1_v2_optimized -902 against +7188, s4_optimized_v2 -1314 against +2596. Their 24-month net is therefore one profitable year
  * paying for one losing one, which a single pooled figure cannot show. That year is the reason s6 exists and the constraint it was chosen
  * under. It also cuts the other way: the vals that survive it are the counter-trend ones, and they are the ones that earn least on the
  * holdout, so neither column alone ranks the file. Note that s2_optimized predates the six-fold corpus - when it was selected the folds
  * spanned one year, so nothing in its search ever scored it on 2023-24.
  *
  * Both lines were re-measured on 2026-08-25 when the folds went from three to six and reporting moved to the holdout, and re-verified
  * unchanged on 2026-08-27. They are not comparable to figures quoted in commit history or in `ga-optimisation-*.md` reports before
  * 2026-08-25: the earlier "searched" column was one year rather than two, and the earlier out-of-sample column was the whole newer export,
  * a third of which is the validation fold every champion's finalist ranking selected on.
  *
  * Version suffixes neither rank a family nor run contiguously within one. The catalogue was pruned to its measured winners on 2026-08-27,
  * and where a GA descendant beat the base it came from, the descendant was promoted into the base's name and the base deleted — so
  * `s1_v2_optimized` here is the champion of ga-optimisation-2026-08-24-1855, not the val that report was searching. The same prune left
  * `s5_optimized_v2` as the only member of its family, with no `s5_optimized` above it. A suffix therefore records only that a val once
  * needed distinguishing from something; the report filename in each comment is the stable link back, and the holdout line is the ranking.
  *
  * One val survives the sixteen rounds of 2026-08-25/26: s5_optimized_v2, which ranked ninth of the fourteen champions those rounds
  * selected and breached five constraints, and which now posts the best holdout profit factor and Sharpe in this file. Of the other
  * thirteen, two beat the base they came from and the rest were deleted; nine had a training fitness of exactly 0.000000, meaning the six
  * search folds scored them at zero and the four-month validation fold ranked them alone, and the round with the highest validation fitness
  * and the only clean constraint verdict of the batch returned a quarter of its base's holdout net. Two rounds selected nothing at all.
  * Treat a validation figure from that generation of rounds as a filter that rejects, not as a ranking.
  */
object TestStrategy {

  // GA-optimized indicator params for s1_v2_optimized, which is no longer in this catalogue (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-1855-s1_v2_optimized_shuffle.md (training 1.622058 -> validation 0.125887, retaining 7.8%, shuffled GA).
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.533, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - profit factor is 1.11609, required >= 1.2
  // Promoted into its base's name on 2026-08-27, having beaten it on both corpora (1536 vs 1251 on the holdout) at a lower drawdown,
  // despite the breaches above.
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

  // GA-optimized indicator params for s2_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-1811-s2_optimized.md (training 1.430220 -> validation 1.043568, retaining 73.0%).
  // Satisfies every constraint on validation data — the only champion of the 2026-08-24/25 batch that does.
  // Best holdout net in the catalogue, and second-best Sharpe behind s5_optimized_v2: it beats s2_optimized on data neither was selected on
  // (2660 vs 2480, Sharpe 2.628 vs 2.142)
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

  // GA-optimized indicator params for s4_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-08-03-1820-s4_optimized.md (training 1.658671 -> validation 1.675786).
  // Satisfies every constraint on validation data.
  // searched 2023-07..2025-07: net=3435.55301, closed=467, forced=4, win=69.59%, exp=7.356645, PF=1.621, DD=0.54%, Sharpe=1.984
  // holdout 2025-12..2026-06:  net=622.99662, closed=141, forced=1, win=73.76%, exp=4.418416, PF=1.363, DD=0.49%, Sharpe=1.771
  val s4_optimized_v1 = TestStrategy(
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

  // GA-optimized indicator params for s4_optimized_v1 (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-2107-s4_optimized_v1_shuffle.md (training 1.057177 -> validation 0.485363, retaining 45.9%, shuffled GA).
  // BREACHES 1 constraint(s) on validation data:
  //   - most concentrated pair's best month is 0.776, required <= 0.755 (0.700 scaled to 4 periods)
  // The most interesting result of the batch. It gives up most of its in-sample net against s4_optimized_v1 (1281 vs 3436) and beats it on
  // the holdout (1080 vs 623), where it posts the second-best profit factor in the catalogue (1.565, behind s5_optimized_v2) and the
  // third-best Sharpe (2.189) at a 0.72% drawdown.
  // One of only four vals whose profit factor is higher on the holdout than in sample, and by far the widest gap of them (1.155 -> 1.565):
  // whatever the GA found here, it was not a fit to the folds.
  // searched 2023-07..2025-07: net=1281.32473, closed=788, forced=2, win=57.11%, exp=1.626047, PF=1.155, DD=1.25%, Sharpe=0.669
  // holdout 2025-12..2026-06:  net=1079.89722, closed=210, forced=0, win=61.43%, exp=5.142368, PF=1.565, DD=0.72%, Sharpe=2.189
  val s4_optimized_v2 = TestStrategy(
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

  // GA-optimized indicator params for s5_optimized, which is no longer in this catalogue (rules unchanged). Champion from
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
  // 0.49% drawdown. Beat the s5_optimized it came from on both corpora — 4222 vs 3089 in sample, 1607 vs 705 out — and its holdout PF is
  // higher than its in-sample PF, so the improvement is not a fit to the folds. It kept its `_v2` suffix when that base was deleted, and is
  // now the only member of its family.
  // Its GA fitness said none of this: 0.106525 on validation with five breached constraints, ninth of the fourteen champions measured. The
  // previous batch's s5 champion taught the same lesson from the same base, which makes this the s5 family's pattern rather than one fluke.
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

  // S6: Bollinger re-entry and squeeze breakout, banked at the opposite momentum extreme.
  //
  // Two ways in, sharing one exit. The breakout leg is s5's: with the slow JMA trend up and ATR below its own average, a close
  // through the upper band is a squeeze resolving in the trend's direction. The reversion leg is the earner: price that has been
  // outside the lower band closing back inside it, while RSX is turning up, is an overextension being given up. Both are exited the
  // same way, when RSX crosses into the zone opposite the position - the only exit here, and the one the profit comes from.
  //
  // Three departures from s5_optimized_v2, each measured on its own. Figures are searched-corpus net with the two years scored
  // separately and summed, at trend 80 / ATR 20 / SMA 50 / mult 2.6 / RSX 11 66-30 and the trend exit still in place, which scores
  // 6287, unless another basis is named:
  //   - the reversion leg asks momentum to be turning (a state) rather than to have just left an extreme zone (an event). s5 needs
  //     the band crossing and the zone change on the same bar, and that coincidence, not the idea, is what holds it to 342 trades
  //     over the 24 months. Loosening it is worth +1613 (4347 -> 5960, measured on s5's own ATR 28 / SMA 63). On s5's parameters the
  //     reversion leg alone earns 2779 of its 4222 and the breakout leg alone 1050, so the reversion leg is the bulk of the strategy.
  //   - no exit on the trend reversing against the position. It was cutting winners: removing it is worth +824 (6287 -> 7111) on 109
  //     fewer trades, and +664 at the trend 90 finally chosen (6698 -> 7362). Removing the momentum exit instead costs 4255
  //     (6287 -> 2032), and dropping the squeeze from the breakout leg costs 5481 (6287 -> 806). Those two carry the strategy.
  //   - a slower trend (JMA 90 rather than 50) and a slower squeeze (ATR 20 against SMA 50 rather than 28/63). Both sit on smooth
  //     ridges rather than spikes: trend lengths 70, 80, 85, 95, 100 and 110 all return between 6289 and 7269.
  //
  // Chosen by coarse grid on the two searched years scored SEPARATELY, requiring both to be profitable - see the docstring's note on
  // the 2023-24 year, which most of this file loses money in. Four other vals clear that bar (s5_optimized_v2, s2_optimized_v2,
  // s12_optimized and s12); s6 earns more in sample than any of them, and more than anything else here. It does NOT lead on the holdout:
  // read the caveat below before putting it on an account.
  //
  // Not GA-optimized, hence no _optimized suffix and no report to link to. A 27-point one-at-a-time perturbation sweep around the
  // chosen parameters (trend length, band length, deviation length, band multiplier, RSX length, and the squeeze pair) left every
  // variant profitable in both searched years, in a band of 3863 to 7269 in-sample net. The band multiplier is the one sensitive
  // parameter: 2.6 earns 7362, 2.5 earns 4635 and 2.4 earns 3863, mostly by collapsing the second year. Treat 2.6 as fitted and the
  // rest as structural.
  //
  // CAVEAT, and the reason this val is not a straight upgrade on s2_optimized_v3: it earns far less out of sample than in. Per month
  // it makes 307 in sample against 74 across the validation fold and the holdout, while s2_optimized moves the other way, 243 in
  // sample against 345 out. Its profit factor stays above 1 in all four periods (1.69, 1.64, 1.11, 1.16), so it does not break out of
  // sample, it just earns thinly there - and the searched years are exactly the data it was chosen on. What it demonstrates is that
  // the 2023-24 year is survivable, not that this is the strongest forward bet in the file.
  // searched 2023-07..2025-07: net=7362.30479, closed=674, forced=9, win=70.03%, exp=10.923301, PF=1.667, DD=0.59%, Sharpe=2.565
  //   of which 2023-07..2024-06: net=3844, closed=346, PF=1.69   and 2024-07..2025-07: net=3518, closed=328, PF=1.64
  // holdout 2025-12..2026-06:  net=559.56308, closed=176, forced=2, win=65.91%, exp=3.179336, PF=1.163, DD=1.26%, Sharpe=0.869
  val s6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      // Regime: the slow trend the breakout leg has to agree with.
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 90, phase = -6, power = 1)
      ),
      // The channel both entry legs read, as a breakout through it or a re-entry back into it.
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 41,
        stdDevMultiplier = 2.6
      ),
      // Squeeze: gates the breakout leg only. Removing it from that leg costs 6500 in sample.
      Indicator.VolatilityRegimeDetection(
        atrLength = 20,
        smoothingType = ValueTransformation.SMA(length = 50)
      ),
      // Drives the momentum zone, and so the exit.
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 11),
        upperBoundary = 66.0,
        lowerBoundary = 30.0
      ),
      // Last, so this owns lastMomentumValue rather than the ThresholdCrossing above it, which only writes on a crossing.
      // MomentumIs reads it to tell a turn from a drift. The length barely matters: RSX 5, 8 and 12 all agree on direction
      // bar to bar and score identically.
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
              // Squeeze resolving upward with the trend.
              Rule.Condition.allOf(
                Rule.Condition.trendIsUpward,
                Rule.Condition.volatilityIsLow,
                Rule.Condition.UpperBandCrossed(Direction.Upward)
              ),
              // Price back inside the channel from below, momentum turning up.
              Rule.Condition.allOf(
                Rule.Condition.LowerBandCrossed(Direction.Upward),
                Rule.Condition.MomentumIs(Direction.Upward)
              )
            )
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.anyOf(
              Rule.Condition.allOf(
                Rule.Condition.trendIsDownward,
                Rule.Condition.volatilityIsLow,
                Rule.Condition.LowerBandCrossed(Direction.Downward)
              ),
              Rule.Condition.allOf(
                Rule.Condition.UpperBandCrossed(Direction.Downward),
                Rule.Condition.MomentumIs(Direction.Downward)
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

  // GA-optimized indicator params for s6 (rules unchanged). Best by validation from ga-optimisation-2026-09-02-1428-s6.md (NOTHING SELECTED) (training 1.104624 -> validation 0.000000, retaining n/a).
  // NOTHING SELECTED: no finalist scored above zero on validation data.
  // searched 2023-07..2025-07: net=6104.83401, closed=757, forced=2, win=70.41%, exp=8.064510, PF=1.836, DD=0.41%, Sharpe=4.021
  // holdout 2025-12..2026-06:  net=495.66886, closed=201, forced=2, win=65.17%, exp=2.466014, PF=1.166, DD=1.11%, Sharpe=1.426
  val s6_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 96, phase = -37, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 32),
        stdDevLength = 34,
        stdDevMultiplier = 2.7
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 20,
        smoothingType = ValueTransformation.SMA(length = 48)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 69.0,
        lowerBoundary = 30.0
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
              // Squeeze resolving upward with the trend.
              Rule.Condition.allOf(
                Rule.Condition.trendIsUpward,
                Rule.Condition.volatilityIsLow,
                Rule.Condition.UpperBandCrossed(Direction.Upward)
              ),
              // Price back inside the channel from below, momentum turning up.
              Rule.Condition.allOf(
                Rule.Condition.LowerBandCrossed(Direction.Upward),
                Rule.Condition.MomentumIs(Direction.Upward)
              )
            )
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.anyOf(
              Rule.Condition.allOf(
                Rule.Condition.trendIsDownward,
                Rule.Condition.volatilityIsLow,
                Rule.Condition.LowerBandCrossed(Direction.Downward)
              ),
              Rule.Condition.allOf(
                Rule.Condition.UpperBandCrossed(Direction.Downward),
                Rule.Condition.MomentumIs(Direction.Downward)
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

  // GA-optimized indicator params for s6 (rules unchanged). Champion from ga-optimisation-2026-09-02-1537-s6_shuffle.md (training 0.840683 -> validation 0.263712, retaining 31.4%, shuffled GA).
  // BREACHES 1 constraint(s) on validation data:
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  // searched 2023-07..2025-07: net=6012.64506, closed=851, forced=6, win=69.92%, exp=7.065388, PF=1.832, DD=0.53%, Sharpe=3.391
  // holdout 2025-12..2026-06:  net=17.23606, closed=225, forced=1, win=64.44%, exp=0.076605, PF=1.006, DD=1.93%, Sharpe=0.031
  val s6_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 96, phase = -16, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 23,
        smoothingType = ValueTransformation.SMA(length = 68)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 5),
        upperBoundary = 66.0,
        lowerBoundary = 30.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 13)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.anyOf(
              // Squeeze resolving upward with the trend.
              Rule.Condition.allOf(
                Rule.Condition.trendIsUpward,
                Rule.Condition.volatilityIsLow,
                Rule.Condition.UpperBandCrossed(Direction.Upward)
              ),
              // Price back inside the channel from below, momentum turning up.
              Rule.Condition.allOf(
                Rule.Condition.LowerBandCrossed(Direction.Upward),
                Rule.Condition.MomentumIs(Direction.Upward)
              )
            )
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.anyOf(
              Rule.Condition.allOf(
                Rule.Condition.trendIsDownward,
                Rule.Condition.volatilityIsLow,
                Rule.Condition.LowerBandCrossed(Direction.Downward)
              ),
              Rule.Condition.allOf(
                Rule.Condition.UpperBandCrossed(Direction.Downward),
                Rule.Condition.MomentumIs(Direction.Downward)
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

  // GA-optimized indicator params for s6 (rules unchanged). Training fitness leader from ga-optimisation-2026-09-02-1537-s6_shuffle.md (training 1.173006 -> validation 0.000000, retaining n/a, shuffled GA).
  // searched 2023-07..2025-07: net=6721.17613, closed=816, forced=6, win=68.87%, exp=8.236735, PF=1.857, DD=0.49%, Sharpe=4.363
  // holdout 2025-12..2026-06:  net=272.29990, closed=217, forced=1, win=67.28%, exp=1.254838, PF=1.090, DD=2.07%, Sharpe=0.347
  val s6_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 96, phase = -16, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 23,
        smoothingType = ValueTransformation.SMA(length = 68)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 64.0,
        lowerBoundary = 28.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 23)
      )
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.anyOf(
              // Squeeze resolving upward with the trend.
              Rule.Condition.allOf(
                Rule.Condition.trendIsUpward,
                Rule.Condition.volatilityIsLow,
                Rule.Condition.UpperBandCrossed(Direction.Upward)
              ),
              // Price back inside the channel from below, momentum turning up.
              Rule.Condition.allOf(
                Rule.Condition.LowerBandCrossed(Direction.Upward),
                Rule.Condition.MomentumIs(Direction.Upward)
              )
            )
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.anyOf(
              Rule.Condition.allOf(
                Rule.Condition.trendIsDownward,
                Rule.Condition.volatilityIsLow,
                Rule.Condition.LowerBandCrossed(Direction.Downward)
              ),
              Rule.Condition.allOf(
                Rule.Condition.UpperBandCrossed(Direction.Downward),
                Rule.Condition.MomentumIs(Direction.Downward)
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

  // GA-optimized indicator params for s2_optimized (rules unchanged). Champion from ga-optimisation-2026-09-02-1642-s2_optimized.md (training 0.505873 -> validation 0.515308, retaining 101.9%).
  // BREACHES 2 constraint(s) on validation data:
  //   - profitable pair-months is 0.467, required >= 0.550
  //   - most concentrated pair's best month is 0.749, required <= 0.738 (0.700 scaled to 5 periods)
  // searched 2023-07..2025-07: net=9080.90952, closed=1841, forced=11, win=40.09%, exp=4.932596, PF=1.347, DD=1.90%, Sharpe=1.965
  // holdout 2025-12..2026-06:   net=2180.13789, closed=524, forced=6, win=43.13%, exp=4.160569, PF=1.291, DD=1.52%, Sharpe=2.116
  val s2_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 3, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 23, phase = -6, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 31.0
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

  // GA-optimized indicator params for s2_optimized (rules unchanged). Training fitness leader from ga-optimisation-2026-09-02-1642-s2_optimized.md (training 0.633197 -> validation 0.170544, retaining 26.9%).
  // searched 2023-07..2025-07: net=9986.17231, closed=1687, forced=12, win=40.07%, exp=5.919486, PF=1.419, DD=1.33%, Sharpe=2.402
  // holdout 2025-12..2026-06:   net=2518.28498, closed=443, forced=6, win=43.34%, exp=5.684616, PF=1.383, DD=1.62%, Sharpe=2.285
  val s2_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 23, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 24, phase = -2, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 26.0
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

  // GA-optimized indicator params for s2_optimized (rules unchanged). Best by validation from ga-optimisation-2026-09-02-1712-s2_optimized_shuffle.md (NOTHING SELECTED) (training 0.928524 -> validation 0.000000, retaining n/a, shuffled GA).
  // NOTHING SELECTED: no finalist scored above zero on validation data.
  // searched 2023-07..2025-07: net=9866.81063, closed=1776, forced=12, win=41.72%, exp=5.555637, PF=1.411, DD=0.81%, Sharpe=3.433
  // holdout 2025-12..2026-06:   net=2090.24989, closed=516, forced=6, win=43.99%, exp=4.050872, PF=1.276, DD=1.92%, Sharpe=2.495
  val s2_optimized_v6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 10, phase = -37, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 16, phase = -33, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 15.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 37,
        smoothingType = ValueTransformation.SMA(length = 39)
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged). Champion from ga-optimisation-2026-09-02-1743-s5_optimized_v2.md (training 0.675135 -> validation 0.001892, retaining 0.3%).
  // BREACHES 6 constraint(s) on validation data:
  //   - profitable pair-months is 0.370, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - pair-month profit factor is 1.098380312928659645957379057388344, required >= 1.3
  //   - profit factor is 1.03897, required >= 1.2
  //   - costs as a share of gross profit is 0.659, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // searched 2023-07..2025-07: net=6075.49099, closed=784, forced=5, win=66.33%, exp=7.749351, PF=1.664, DD=0.48%, Sharpe=3.682
  // holdout 2025-12..2026-06:   net=1281.96884, closed=227, forced=0, win=67.84%, exp=5.647440, PF=1.428, DD=0.54%, Sharpe=4.935
  val s5_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 58, phase = 21, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 35,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 24,
        smoothingType = ValueTransformation.SMA(length = 61)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 68.0,
        lowerBoundary = 29.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 12)
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged). Training fitness leader from ga-optimisation-2026-09-02-1743-s5_optimized_v2.md (training 1.055440 -> validation 0.000000, retaining n/a).
  // searched 2023-07..2025-07: net=8209.76883, closed=774, forced=5, win=68.35%, exp=10.606936, PF=2.065, DD=0.34%, Sharpe=4.959
  // holdout 2025-12..2026-06:   net=1100.10399, closed=235, forced=0, win=65.53%, exp=4.681294, PF=1.379, DD=0.67%, Sharpe=2.203
  val s5_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 58, phase = 8, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 33),
        stdDevLength = 34,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 24,
        smoothingType = ValueTransformation.SMA(length = 55)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 68.0,
        lowerBoundary = 29.0
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged). Best by validation from ga-optimisation-2026-09-02-1850-s5_optimized_v2_shuffle.md (NOTHING SELECTED) (training 1.069241 -> validation 0.000000, retaining n/a, shuffled GA).
  // NOTHING SELECTED: no finalist scored above zero on validation data.
  // searched 2023-07..2025-07: net=5850.05309, closed=803, forced=5, win=64.63%, exp=7.285247, PF=1.665, DD=0.33%, Sharpe=4.310
  // holdout 2025-12..2026-06:  net=275.91228, closed=249, forced=1, win=56.63%, exp=1.108081, PF=1.074, DD=1.30%, Sharpe=0.358
  val s5_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 79, phase = 15, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 43),
        stdDevLength = 47,
        stdDevMultiplier = 2.2
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 22,
        smoothingType = ValueTransformation.SMA(length = 49)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 12),
        upperBoundary = 65.0,
        lowerBoundary = 33.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 12)
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged). Champion from ga-optimisation-2026-09-02-2000-s1_v2_optimized.md (training 0.349408 -> validation 0.007407, retaining 2.1%).
  // BREACHES 6 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - pair-month profit factor is 1.142499018369833090937010486340065, required >= 1.3
  //   - profit factor is 1.05245, required >= 1.2
  //   - costs as a share of gross profit is 0.517, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // searched 2023-07..2025-07: net=6550.27593, closed=1354, forced=11, win=46.82%, exp=4.837722, PF=1.269, DD=1.45%, Sharpe=1.810
  // holdout 2025-12..2026-06:  net=681.60688, closed=380, forced=6, win=45.00%, exp=1.793702, PF=1.101, DD=1.47%, Sharpe=0.593
  val s1_v2_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 19, phase = -14, power = 3),
        line2Transformation = ValueTransformation.JMA(length = 40, phase = -16, power = 3)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 24),
        upperBoundary = 70.0,
        lowerBoundary = 29.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 9)
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 26,
        smoothingType = ValueTransformation.SMA(length = 38)
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged). Training fitness leader from ga-optimisation-2026-09-02-2000-s1_v2_optimized.md (training 0.506532 -> validation 0.000000, retaining n/a).
  // searched 2023-07..2025-07: net=6290.16575, closed=1296, forced=10, win=46.37%, exp=4.853523, PF=1.262, DD=2.34%, Sharpe=1.653
  // holdout 2025-12..2026-06:  net=-3.28209, closed=345, forced=6, win=44.35%, exp=-0.009513, PF=1.000, DD=1.86%, Sharpe=0.005
  val s1_v2_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 19, phase = -14, power = 3),
        line2Transformation = ValueTransformation.JMA(length = 41, phase = -14, power = 3)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 25),
        upperBoundary = 71.0,
        lowerBoundary = 30.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16)
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 26,
        smoothingType = ValueTransformation.SMA(length = 48)
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged). Champion from ga-optimisation-2026-09-02-2037-s1_v2_optimized_shuffle.md (training 0.310274 -> validation 0.387754, retaining 125.0%, shuffled GA).
  // BREACHES 2 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  // searched 2023-07..2025-07: net=3069.29363, closed=1383, forced=10, win=42.59%, exp=2.219301, PF=1.116, DD=2.15%, Sharpe=0.964
  // holdout 2025-12..2026-06:  net=-634.31923, closed=394, forced=5, win=42.13%, exp=-1.609947, PF=0.916, DD=1.76%, Sharpe=-0.648
  val s1_v2_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 19, phase = 11, power = 4),
        line2Transformation = ValueTransformation.JMA(length = 46, phase = -7, power = 3)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 27),
        upperBoundary = 70.0,
        lowerBoundary = 30.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 32)
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 28,
        smoothingType = ValueTransformation.SMA(length = 46)
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged). Training fitness leader from ga-optimisation-2026-09-02-2037-s1_v2_optimized_shuffle.md (training 0.589565 -> validation 0.000000, retaining n/a, shuffled GA).
  // searched 2023-07..2025-07: net=7947.01979, closed=1426, forced=10, win=44.74%, exp=5.572945, PF=1.343, DD=1.02%, Sharpe=1.967
  // holdout 2025-12..2026-06:  net=293.59424, closed=413, forced=6, win=44.07%, exp=0.710882, PF=1.039, DD=1.03%, Sharpe=0.339
  val s1_v2_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 18, phase = 29, power = 4),
        line2Transformation = ValueTransformation.JMA(length = 45, phase = -16, power = 3)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 66.0,
        lowerBoundary = 30.0
      ),
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 32)
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 27,
        smoothingType = ValueTransformation.SMA(length = 38)
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

  // GA-optimized indicator params for s2_optimized_v2 (rules unchanged). Champion from ga-optimisation-2026-09-02-2117-s2_optimized_v2.md (training 0.501863 -> validation 0.263994, retaining 52.6%).
  // BREACHES 2 constraint(s) on validation data:
  //   - pair-month profit factor is 1.270008488158519250022056836279097, required >= 1.3
  //   - profit factor is 1.16911, required >= 1.2
  // searched 2023-07..2025-07: net=6227.27069, closed=989, forced=8, win=71.79%, exp=6.296533, PF=1.445, DD=0.91%, Sharpe=2.214
  // holdout 2025-12..2026-06:  net=278.91165, closed=270, forced=2, win=70.37%, exp=1.033006, PF=1.061, DD=1.23%, Sharpe=0.571
  val s2_optimized_v7 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 38, phase = -33, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 24, phase = -2, power = 6)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 26),
        upperBoundary = 55.0,
        lowerBoundary = 10.0
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

  // GA-optimized indicator params for s2_optimized_v2 (rules unchanged). Training fitness leader from ga-optimisation-2026-09-02-2117-s2_optimized_v2.md (training 0.736645 -> validation 0.000000, retaining n/a).
  // searched 2023-07..2025-07: net=6702.20256, closed=947, forced=7, win=74.55%, exp=7.077299, PF=1.564, DD=0.81%, Sharpe=2.820
  // holdout 2025-12..2026-06:  net=36.31750, closed=265, forced=2, win=70.94%, exp=0.137047, PF=1.008, DD=1.27%, Sharpe=0.077
  val s2_optimized_v8 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 38, phase = -35, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 24, phase = -9, power = 6)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 26),
        upperBoundary = 54.0,
        lowerBoundary = 8.0
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

  // GA-optimized indicator params for s2_optimized_v2 (rules unchanged). Champion from ga-optimisation-2026-09-02-2147-s2_optimized_v2_shuffle.md (training 1.021368 -> validation 0.027259, retaining 2.7%, shuffled GA).
  // BREACHES 5 constraint(s) on validation data:
  //   - profitable pair-months is 0.433, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - pair-month profit factor is 1.245394495792424942449527363175293, required >= 1.3
  //   - profit factor is 1.06886, required >= 1.2
  //   - costs as a share of gross profit is 0.528, required <= 0.400
  // searched 2023-07..2025-07: net=10619.61338, closed=1690, forced=12, win=40.36%, exp=6.283795, PF=1.464, DD=0.90%, Sharpe=3.296
  // holdout 2025-12..2026-06:   net=2129.21743, closed=482, forced=6, win=39.83%, exp=4.417464, PF=1.299, DD=1.35%, Sharpe=2.507
  val s2_optimized_v9 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 23, phase = 99, power = 3),
        line2Transformation = ValueTransformation.JMA(length = 34, phase = -36, power = 2)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 25.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 37,
        smoothingType = ValueTransformation.SMA(length = 40)
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

  // GA-optimized indicator params for s2_optimized_v2 (rules unchanged). Training fitness leader from ga-optimisation-2026-09-02-2147-s2_optimized_v2_shuffle.md (training 1.181125 -> validation 0.000000, retaining n/a, shuffled GA).
  // searched 2023-07..2025-07: net=12382.41623, closed=1699, forced=12, win=40.55%, exp=7.288061, PF=1.553, DD=1.02%, Sharpe=3.665
  // holdout 2025-12..2026-06:   net=2301.68602, closed=457, forced=6, win=39.61%, exp=5.036512, PF=1.324, DD=1.04%, Sharpe=3.048
  val s2_optimized_v10 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 22, phase = 100, power = 3),
        line2Transformation = ValueTransformation.JMA(length = 36, phase = -37, power = 2)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 24.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 37,
        smoothingType = ValueTransformation.SMA(length = 40)
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

  // GA-optimized indicator params for s4_optimized_v1 (rules unchanged). Champion from ga-optimisation-2026-09-02-2219-s4_optimized_v1.md (training 0.158996 -> validation 0.284267, retaining 178.8%).
  // BREACHES 3 constraint(s) on validation data:
  //   - closed trades is 94, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - profitable pair-months is 0.542, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  // searched 2023-07..2025-07: net=1266.71041, closed=479, forced=2, win=63.26%, exp=2.644489, PF=1.172, DD=0.76%, Sharpe=0.764
  // holdout 2025-12..2026-06:  net=100.68357, closed=137, forced=2, win=64.96%, exp=0.734917, PF=1.046, DD=0.74%, Sharpe=0.285
  val s4_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 56, phase = -64, power = 1)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 21),
        atrLength = 15,
        atrMultiplier = 2.3
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 73.0,
        lowerBoundary = 26.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 32,
        smoothingType = ValueTransformation.SMA(length = 52)
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

  // GA-optimized indicator params for s4_optimized_v1 (rules unchanged). Training fitness leader from ga-optimisation-2026-09-02-2219-s4_optimized_v1.md (training 0.382194 -> validation 0.000000, retaining n/a).
  // searched 2023-07..2025-07: net=2668.24131, closed=482, forced=3, win=67.22%, exp=5.535770, PF=1.479, DD=0.27%, Sharpe=2.310
  // holdout 2025-12..2026-06:  net=992.33004, closed=135, forced=2, win=73.33%, exp=7.350593, PF=1.684, DD=0.58%, Sharpe=2.924
  val s4_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 48, phase = -61, power = 1)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 21),
        atrLength = 13,
        atrMultiplier = 2.4
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 11),
        upperBoundary = 74.0,
        lowerBoundary = 30.0
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 32,
        smoothingType = ValueTransformation.SMA(length = 52)
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

  // GA-optimized indicator params for s4_optimized_v1 (rules unchanged). Best by validation from ga-optimisation-2026-09-02-2305-s4_optimized_v1_shuffle.md (NOTHING SELECTED) (training 0.500224 -> validation 0.000000, retaining n/a, shuffled GA).
  // NOTHING SELECTED: no finalist scored above zero on validation data.
  // searched 2023-07..2025-07: net=3631.17661, closed=599, forced=3, win=72.29%, exp=6.062064, PF=1.559, DD=0.41%, Sharpe=2.082
  // holdout 2025-12..2026-06:  net=637.50743, closed=161, forced=2, win=73.29%, exp=3.959673, PF=1.339, DD=0.89%, Sharpe=1.053
  val s4_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 50, phase = -73, power = 1)
      ),
      Indicator.KeltnerChannel(
        source = ValueSource.Close,
        middleBand = ValueTransformation.EMA(length = 28),
        atrLength = 21,
        atrMultiplier = 2.5
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 13),
        upperBoundary = 70.0,
        lowerBoundary = 33.0
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
}
