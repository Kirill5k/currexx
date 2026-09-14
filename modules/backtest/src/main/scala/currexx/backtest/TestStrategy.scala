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
  * the GA folds cover, so for anything named `_optimized` it reports fit to the data that chose it and is not evidence of an edge. Older
  * comments call 2025-12..2026-06 the holdout. The s10_v2 follow-up reuses that period for development, explicitly labelled `historical`;
  * its result is not independent validation. Sharp disagreement between searched and later results can indicate fitting to the search data.
  * An older example is s12_optimized, PF 1.400 in-sample against 0.809 out, on a corpus where its whole family loses money.
  * `BatchBacktester` prints the two searched years separately as well as pooled; vals carry a `searched 2023-07..2024-06` line where the
  * split matters to the decision.
  *
  * Read the later evaluation column across strategies, not as a forecast. Its net figures cover seven months against the searched column's
  * twenty-four, so they are not comparable to each other. Compare strategies within the same period and respect each val's selection
  * history; the s10_v2 result is fitted to the reused evaluation period.
  *
  * The 2026-09-14 batch measured 36 distinct report top #1 and training-fitness leaders. After comparison and cleanup, only
  * s10_optimized_v7 survives, promoted into s10 and replacing the original definition. The other 35 candidates were deleted. See
  * docs/ga-promotions-2026-09-14.md for all measurements and decisions, including the deleted candidates. Older comparative prose and
  * metrics record their original promotion dates; current DD and Sharpe may differ as the statistics evolve.
  *
  * Not every val here is still measured. `BatchBacktester` holds the ones worth the runtime and is the list, rather than a copy of it kept
  * in this comment; a val it has dropped carries a `Not in BatchBacktester` line saying which val dominates it. The rest stay so that a
  * report filename still resolves to the thing it selected.
  *
  * `s4_optimized_v2` shows the anti-overfit pattern most clearly: its holdout profit factor beats its in-sample one. `s5_optimized_v2`
  * leads the file on holdout profit factor and Sharpe, `s2_optimized_v4` on holdout net. The earlier September promotions
  * (`s2_optimized_v4`, `s5_optimized_v3`) were training-fitness leaders whose validation figure was 0.000000, which is the standing
  * reminder that validation ranking is a filter that rejects and not a scoreboard that ranks.
  *
  * The searched column hides a split worth knowing about, which is why the two years are reported separately. It was once a clean division
  * — every JMA-crossover val lost money in 2023-07..2024-06 while the counter-trend ones survived it, and that year is the reason s6
  * exists. It no longer is: `s2_optimized_v4` nets +4245 there and `s5_optimized_v3` +3656, so the split now separates the vals that were
  * searched under a fitness that scored 2023-24 as a fold from the older ones that were not. Neither column alone ranks the file.
  *
  * Version suffixes record only that a val once needed distinguishing from something; the report filename in each comment is the stable
  * link back; each evaluation line must be read with its selection history.
  */
object TestStrategy {

  // S10 v2: Require a band re-entry to coincide with RSX entering neutral; use the production s5 volatility regime.
  //
  // Two changes to the original s10: replace re-entry momentum direction with MomentumEntered(Neutral), and replace the breakout squeeze
  // ATR(20)/SMA(50) with ATR(28)/SMA(63). Keep the slow trend, Bollinger bands, momentum profit-taking and four-current-ATR exit.
  // Neutral is a directionless zone transition; the band crossing supplies the trade direction. Breakout band/trend conditions are unchanged.
  // Price/ATR trackers refresh the profile; the unused RSX(8) value tracker is removed after exact completed-trade comparison.
  //
  // Manual follow-up to the rejected original s10. The user requested at least USD 1000 on its December-June comparison; that previously
  // viewed holdout was explicitly reused for development. These are fitted historical results, NOT new out-of-sample evidence.
  // Confirmation alone: historical net 969.38; regime alone: 1125.42; both: 1613.32, versus the original s10's 527.10 at identical sizing.
  // Ten one-at-a-time perturbations (ATR 26/30, smoothing 58/68, stop 3/5, upper threshold 64/68, lower 28/32) remained profitable
  // in both searched years and returned 1169.47..1662.63 historically. Kept the original combined candidate rather than a local peak.
  // Both entry legs contribute: historical net falls to 938.68 without breakout or 638.27 without re-entry.
  // Removing the price exit gives 1439.57 historically and ADDS 1048.51 in the searched years; its effect is not uniformly positive.
  //
  // Historical diagnostics: all six pairs and both sides profitable; 5/7 positive months; net excluding the best five trades 1095.31;
  // zero forced closures. Fails the minimum trade-count constraint (132 vs 210); no fresh data beyond June 2026 was available locally.
  // Both searched years also fail the trade-count floor; the first has 54.1% profitable pair-months, just below the required 55%.
  // This is a research candidate, not a demonstrated upgrade on s5_optimized_v2 (historical PF 2.079 and DD 0.49%).
  // The four-ATR exit uses the current ATR and next-bar market execution, not a broker stop or a fixed loss cap.
  // See docs/s10-improvement-2026-09-10.md for all trials, reused-data disclosure and production comparisons.
  // searched 2023-07..2024-06: net=1321.56132, closed=241, forced=2, win=63.90%, exp=5.483657, PF=1.315, DD=0.78%, Sharpe=1.949
  // searched 2023-07..2025-07: net=4146.27418, closed=473, forced=3, win=67.65%, exp=8.765907, PF=1.566, DD=0.39%, Sharpe=3.074
  // historical 2025-12..2026-06: net=1613.31781, closed=132, forced=0, win=70.46%, exp=12.222105, PF=2.008, DD=0.77%, Sharpe=2.463
  val s10_v2 = TestStrategy(
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
      // Squeeze gates the breakout leg only.
      Indicator.VolatilityRegimeDetection(
        atrLength = 28,
        smoothingType = ValueTransformation.SMA(length = 63)
      ),
      // ATR and the close independently supply the price-distance exit.
      Indicator.ValueTracking(
        role = ValueRole.Volatility,
        source = ValueSource.Close,
        transformation = ValueTransformation.ATR(length = 14)
      ),
      Indicator.ValueTracking(
        role = ValueRole.Price,
        source = ValueSource.Close,
        // Identity transform: track the raw close, not a smoothed price for the loss exit.
        transformation = ValueTransformation.SMA(length = 1)
      ),
      // Drives the momentum zone, and so the exit.
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 11),
        upperBoundary = 66.0,
        lowerBoundary = 30.0
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
              // Price re-enters from below on the same bar that RSX enters neutral.
              Rule.Condition.allOf(
                Rule.Condition.LowerBandCrossed(Direction.Upward),
                Rule.Condition.MomentumEntered(zone = MomentumZone.Neutral)
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
                Rule.Condition.MomentumEntered(zone = MomentumZone.Neutral)
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
            ),
            Rule.Condition.PriceMovedAgainstEntry(nAtr = 4.0)
          )
        )
      )
    )
  )

  // S10: Bollinger re-entry and squeeze breakout, with momentum profit-taking and a four-current-ATR adverse-price exit.
  // GA-optimized indicator params for the original s10 (rules unchanged). Training fitness leader from
  // ga-optimisation-2026-09-11-2354-s10_shuffle.md
  // training 0.743822 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 3; training rank 1.
  // The report records constraint breaches only for its top #1; this candidate has no recorded constraint verdict.
  // Promoted from s10_optimized_v7 into s10 on 2026-09-14. The original s10 definition and development history are archived
  // in docs/ga-promotions-2026-09-14.md. Both s10 Optimiser rounds now start from these parameters.
  // Holdout net is higher than the original s10:
  // 768.63383 vs 527.09968; PF 1.200 vs 1.115, DD 2.04% vs 1.78%, Sharpe 1.286 vs 1.050.
  // Searched net moves the other way: 5398.63732 vs 5554.43990 for the original base.
  // searched 2023-07..2024-06: net=2498.82688, closed=446, forced=2, win=66.82%, exp=5.602751, PF=1.397, DD=1.05%, Sharpe=2.223
  // searched 2023-07..2025-07: net=5398.63732, closed=860, forced=6, win=67.33%, exp=6.277485, PF=1.448, DD=0.53%, Sharpe=2.600
  // holdout 2025-12..2026-06:  net=768.63383, closed=236, forced=2, win=62.71%, exp=3.256923, PF=1.200, DD=2.04%, Sharpe=1.286
  val s10 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 90, phase = 47, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 41,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = ValueTransformation.SMA(length = 45)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 11)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 9),
        upperBoundary = 66.0,
        lowerBoundary = 36.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 5))
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
            ),
            Rule.Condition.PriceMovedAgainstEntry(nAtr = 4.0)
          )
        )
      )
    )
  )

  // GA-optimized indicator params for s1_v2_optimized, which is no longer in this catalogue (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-1855-s1_v2_optimized_shuffle.md (training 1.622058 -> validation 0.125887, retaining 7.8%, shuffled GA).
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.533, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - profit factor is 1.11609, required >= 1.2
  // Promoted into its base's name on 2026-08-27, having beaten it on both corpora (1536 vs 1251 on the holdout) at a lower drawdown,
  // despite the breaches above.
  // searched 2023-07..2024-06: net=-902.39036, closed=620, forced=6, win=43.87%, exp=-1.455468, PF=0.933, DD=3.56%, Sharpe=-0.712
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
  // Not in BatchBacktester. Survives 2023-07..2024-06 but holdout net (796) is dominated by s2_optimized_v4 (2713).
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

  // GA-optimized indicator params for s4_optimized_v1 (rules unchanged). Champion from
  // ga-optimisation-2026-08-24-2107-s4_optimized_v1_shuffle.md (training 1.057177 -> validation 0.485363, retaining 45.9%, shuffled GA).
  // BREACHES 1 constraint(s) on validation data:
  //   - most concentrated pair's best month is 0.776, required <= 0.755 (0.700 scaled to 4 periods)
  // The most interesting result of the batch. It gives up most of its in-sample net against s4_optimized_v1 (1281 vs 3436) and beats it on
  // the holdout (1080 vs 623), where it posts the second-best profit factor in the catalogue (1.565, behind s5_optimized_v2) and the
  // third-best Sharpe (2.189) at a 0.72% drawdown.
  // One of only four vals whose profit factor is higher on the holdout than in sample, and by far the widest gap of them (1.155 -> 1.565):
  // whatever the GA found here, it was not a fit to the folds.
  // searched 2023-07..2024-06: net=-1314.23615, closed=397, forced=1, win=51.89%, exp=-3.310419, PF=0.728, DD=2.50%, Sharpe=-1.869
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
  // searched 2023-07..2024-06: net=1406.10234, closed=272, forced=3, win=62.13%, exp=5.169494, PF=1.417, DD=0.69%, Sharpe=1.743
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
  // searched 2023-07..2024-06: net=3844.43000, closed=346, forced=5, win=70.23%, exp=11.111069, PF=1.693, DD=1.16%, Sharpe=2.864
  // searched 2023-07..2025-07: net=7362.30479, closed=674, forced=9, win=70.03%, exp=10.923301, PF=1.667, DD=0.59%, Sharpe=2.565
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

  // S13: Bollinger re-entry and trend-aligned squeeze breakout, confirmed by CMF direction and exited at an RSX extreme.
  //
  // CMF(22) replaces s6's price-only RSX direction and also confirms its breakout leg. A long needs rising CMF, a short falling CMF;
  // this does not require a zero crossing or positive CMF for longs. Exit longs on RSX entering overbought, shorts on entering oversold.
  // NoPosition prevents flips. There is no price stop or time cap; orders use the simulator's next-bar execution.
  //
  // Other parameters are inherited from s6 and held fixed. CMF lengths 16/18/20/22/24 all profit in both searched years; 22 improves
  // both years over the initial 20 and has the strongest weaker-year net. Selection used only the searched years. The inherited 2.6
  // Bollinger multiplier remains fitted, as documented on s6; CMF length is manually selected rather than independently validated.
  // At CMF(22), removing the trend, squeeze, breakout, re-entry and RSX exit costs 1180, 2541, 1976, 4480 and 8502 searched net.
  // CMF itself is mixed: -773 in the first year, +898 in the second versus removing its checks, just +126 combined on 96 fewer trades.
  //
  // Historical net modestly beats s6 (685 vs 560) and the earlier CMF strategy s12 (-79), but PF falls from 1.543 searched to 1.217
  // historically. Net per month falls from 248 to 98. The final candidate was fixed before that later evaluation; the period had already
  // been reused by s10_v2, so this is historical evidence, not a pristine holdout. No variants were selected on the later result.
  // Both sides and 4/6 pairs profit historically, with 4/7 positive months. Removing the best five trades leaves +234; best ten leaves
  // -153. GBPUSD supplies 89% of net and shorts 82%; two forced closures contribute just +0.10. Fails the historical trade floor
  // (166 vs 210) and per-pair monthly concentration limit. Both searched years also miss the trade floor (319/299 vs 360 each).
  // Retained as a research strategy. Requires meaningful volume: Oanda supplies it; current AlphaVantage/TwelveData clients do not.
  // See docs/s13-volume-strategy-2026-09-14.md for all manual comparisons, ablations, diagnostics and selection caveats.
  // searched 2023-07..2024-06: net=2924.61298, closed=319, forced=3, win=68.65%, exp=9.168066, PF=1.528, DD=1.56%, Sharpe=2.125
  // searched 2023-07..2025-07: net=5951.97849, closed=618, forced=6, win=68.93%, exp=9.631033, PF=1.543, DD=1.17%, Sharpe=2.012
  // historical 2025-12..2026-06: net=685.30228, closed=166, forced=2, win=65.06%, exp=4.128327, PF=1.217, DD=1.73%, Sharpe=0.978
  val s13 = TestStrategy(
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
      // Squeeze gates the breakout leg only.
      Indicator.VolatilityRegimeDetection(
        atrLength = 20,
        smoothingType = ValueTransformation.SMA(length = 50)
      ),
      // RSX drives the momentum zone used for profit-taking.
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 11),
        upperBoundary = 66.0,
        lowerBoundary = 30.0
      ),
      // Last: CMF owns the current momentum value on every bar; RSX owns only the momentum zone.
      // MomentumIs compares consecutive CMF values, including bars on which RSX crosses a threshold.
      Indicator.ValueTracking(
        role = ValueRole.Momentum,
        source = ValueSource.Close,
        transformation = ValueTransformation.CMF(length = 22)
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
                Rule.Condition.MomentumIs(Direction.Upward),
                Rule.Condition.UpperBandCrossed(Direction.Upward)
              ),
              // Price back inside the channel from below, CMF rising.
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
                Rule.Condition.MomentumIs(Direction.Downward),
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
  // Earlier CMF trend-confirmation design; s13 now uses volume with channel entries and RSX exits. This val is slightly negative on the
  // holdout (net -79, PF 0.976) — the best result of any s12 val, none of which makes money there. The 2026-08-25 rounds gave it the fresh
  // optimisation its transformation-aware threshold bounds called for; the answer was that s12 is searchable but not profitable.
  // Not in BatchBacktester. Negative holdout; needs rule redesign, not more GA.
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
  // Not in BatchBacktester. Loses more on the holdout than s12 does, so it is not even the family's best showing.
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

  // GA-optimized indicator params for s4_optimized_v1 (rules unchanged). Best by validation from ga-optimisation-2026-09-02-2305-s4_optimized_v1_shuffle.md (NOTHING SELECTED) (training 0.500224 -> validation 0.000000, retaining n/a, shuffled GA).
  // NOTHING SELECTED: no finalist scored above zero on validation data.
  // Not in BatchBacktester. Holdout net (637) dominated by s4_optimized_v2 (1080).
  // searched 2023-07..2025-07: net=3631.17661, closed=599, forced=3, win=72.29%, exp=6.062064, PF=1.559, DD=0.41%, Sharpe=2.082
  // holdout 2025-12..2026-06:  net=637.50743, closed=161, forced=2, win=73.29%, exp=3.959673, PF=1.339, DD=0.89%, Sharpe=1.053
  val s4_optimized_v1 = TestStrategy(
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

  // GA-optimized indicator params for s6 (rules unchanged). Training fitness leader from ga-optimisation-2026-09-05-1806-s6_shuffle.md (training 1.049779 -> validation 0.000000, retaining 0.0%), shuffled GA.
  // BREACHES 4 constraint(s) on validation data:
  //   - pair-month profit factor is 1.185271465364436245800896681735918, required >= 1.3
  //   - profit factor is 1.09192, required >= 1.2
  //   - costs as a share of gross profit is 0.508, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // searched 2023-07..2024-06: net=3331.37945, closed=426, forced=1, win=68.78%, exp=7.820140, PF=1.796, DD=0.81%, Sharpe=3.435
  // searched 2023-07..2025-07: net=6242.07098, closed=816, forced=5, win=68.26%, exp=7.649597, PF=1.771, DD=0.42%, Sharpe=3.723
  // holdout 2025-12..2026-06:  net=663.08878, closed=217, forced=2, win=68.20%, exp=3.055709, PF=1.231, DD=1.92%, Sharpe=0.777
  val s6_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(
        source = ValueSource.HLC3,
        transformation = ValueTransformation.JMA(length = 96, phase = -15, power = 1)
      ),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 36),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(
        atrLength = 23,
        smoothingType = ValueTransformation.SMA(length = 67)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 64.0,
        lowerBoundary = 32.0
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

  // GA-optimized indicator params for s2_optimized_v3 (rules unchanged). Training fitness leader from ga-optimisation-2026-09-05-2037-s2_optimized_v3_shuffle.md (training 1.169610 -> validation 0.000000, retaining 0.0%), shuffled GA.
  // BREACHES 6 constraint(s) on validation data:
  //   - profitable pair-months is 0.433, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - pair-month profit factor is 1.179509933850680833559413062545416, required >= 1.3
  //   - profit factor is 1.04883, required >= 1.2
  //   - costs as a share of gross profit is 0.606, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // searched 2023-07..2024-06: net=4245.00844, closed=832, forced=6, win=40.26%, exp=5.102174, PF=1.377, DD=2.01%, Sharpe=2.525
  // searched 2023-07..2025-07: net=12225.94093, closed=1717, forced=12, win=40.01%, exp=7.120525, PF=1.545, DD=1.01%, Sharpe=3.549
  // holdout 2025-12..2026-06:  net=2713.33834, closed=468, forced=6, win=39.32%, exp=5.797731, PF=1.399, DD=0.95%, Sharpe=3.149
  val s2_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 68, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 22, phase = -18, power = 1)
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged). Training fitness leader from ga-optimisation-2026-09-06-0219-s5_optimized_v2_shuffle.md (training 1.128736 -> validation 0.000000, retaining 0.0%), shuffled GA.
  // BREACHES 4 constraint(s) on validation data:
  //   - profitable pair-months is 0.538, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.738 (0.700 scaled to 5 periods)
  //   - profit factor is 1.17852, required >= 1.2
  //   - profitable datasets is 0.500, required >= 0.667
  // searched 2023-07..2024-06: net=3656.62687, closed=381, forced=2, win=66.40%, exp=9.597446, PF=1.964, DD=0.39%, Sharpe=4.748
  // searched 2023-07..2025-07: net=8314.47112, closed=768, forced=5, win=68.49%, exp=10.826134, PF=2.094, DD=0.34%, Sharpe=5.155
  // holdout 2025-12..2026-06:  net=1435.84805, closed=236, forced=0, win=66.53%, exp=6.084102, PF=1.528, DD=0.49%, Sharpe=3.326
  val s5_optimized_v3 = TestStrategy(
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
        atrLength = 26,
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
        transformation = ValueTransformation.RSX(length = 14)
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

}
