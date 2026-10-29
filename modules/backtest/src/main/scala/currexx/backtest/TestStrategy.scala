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
  * The former s12_optimized scored PF 1.400 in-sample against 0.809 out; its replacement and archive are documented below.
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
  * The 2026-09-21 batch measured 34 distinct candidates from the 20 reports dated 2026-09-16..18. After user review, s1_v2_optimized_v7,
  * s6_optimized_v3 and s4_optimized_v6 were promoted into s1_v2_optimized, s6_optimized and s4_optimized_v2, replacing the previous
  * definitions. The retained s10_optimized_v2 was subsequently renamed s10_optimized. The other 30 additions were deleted. All four remain
  * in `BatchBacktester`; full measurements, original names and replaced definitions are archived in docs/ga-promotions-2026-09-21.md.
  * Selection used the already-viewed historical period and does not establish independent validation.
  *
  * The 2026-09-28 batch measured 29 distinct candidates from the 25 reports dated 2026-09-22..25. After user review, s1_v2_optimized_v4 and
  * s13_optimized were retained alongside their bases, and s13_optimized_v5 was promoted into s13, replacing its original definition. The
  * other 26 additions were deleted. All three were included in `BatchBacktester` at that review; measurements, final decisions and the
  * original s13 definition are archived in docs/ga-promotions-2026-09-28.md. The s1 benchmark was removed in the 2026-10-29 cleanup.
  *
  * The 2026-10-06 batch measured 41 distinct candidates from the 30 reports dated 2026-10-04..06. After user review, only s4_optimized_v7
  * remained alongside its base; the other 40 additions were deleted. On 2026-10-11, it replaced s4_optimized_v2 under that stable name. The
  * retained candidate was the highest all-fold fitness reference from its report, not a selected champion. See
  * docs/ga-promotions-2026-10-06.md for measurements, decisions, deleted candidates and the replaced definition.
  *
  * The 2026-10-11 batch measured 36 new diagnostic references from 44 reports dated 2026-10-09..10. After user review, s1_v2_optimized_v10
  * replaces s1_v2_optimized under that stable name; s1_v2_optimized_v4 remained a historic benchmark and search seed until the 2026-10-29
  * cleanup. The other 35 additions were deleted. Every report retained its original base under trial-v1; the user-directed replacement does
  * not change that decision. See docs/ga-promotions-2026-10-11.md for report mappings, rejections, measurements and the replaced
  * definition.
  *
  * The 2026-10-20 batch adds 21 distinct diagnostic references from 19 reports dated 2026-10-19..20: each final top #1 and each distinct
  * highest all-fold search-fitness candidate, skipping exact stored strategies. All reports retained their bases under trial-v1. These
  * version-3 runs optimize complete strategies, so the definitions preserve reported rule parameters as well as indicators. See
  * docs/ga-promotions-2026-10-20.md for the report mapping and continuous batch comparisons. On 2026-10-21, the user selected
  * s13_optimized_v12 to replace s13_optimized under that stable name; the former definition is archived in the same report. The remaining
  * new candidates initially stayed as non-batch diagnostic references, while s12 returned to batch evaluation and optimisation.
  *
  * The 2026-10-29 batch measured 16 diagnostic references from the 21 reports dated 2026-10-24. After user review, s12_optimized_v4
  * replaces s12_optimized, s1_v2_optimized_v19 becomes s1_v2_optimized_v2, and s2_optimized_v19 becomes s2_optimized_v3. Their reported
  * indicators and rule parameters are unchanged. The other 13 additions and the older variants requested for cleanup were deleted. The s1
  * base and s2 base/v2 remain; follow-up cleanup removed s4 variants after v2 and s5 variants after v3. The new s12_optimized joins batch
  * evaluation and its own optimisation rounds alongside s12. All source reports retained their bases under trial-v1; user selection does
  * not change that decision. See docs/ga-promotions-2026-10-29.md for measurements, the rename/deletion mapping and the replaced
  * s12_optimized definition.
  *
  * Register every public strategy val in `StrategyCatalogue`, which controls batch membership and order. Its completeness test catches
  * omitted definitions. All remaining definitions are included in batch evaluation; removed candidates retain their report provenance in
  * the promotion documents.
  *
  * In the earlier catalogue, the original `s4_optimized_v2` had higher holdout profit factor than in sample. `s5_optimized_v2` led on
  * holdout profit factor and Sharpe, and the strategy now named `s2_optimized` led on holdout net. The earlier September promotions
  * (`s2_optimized`, `s5_optimized_v3`) were training-fitness leaders whose validation figure was 0.000000, which is the standing reminder
  * that validation ranking is a filter that rejects and not a scoreboard that ranks.
  *
  * The searched column hides a split worth knowing about, which is why the two years are reported separately. It was once a clean division
  * — every JMA-crossover val lost money in 2023-07..2024-06 while the counter-trend ones survived it, and that year is the reason s6
  * exists. It no longer is: `s2_optimized` nets +4245 there and `s5_optimized_v3` +3656, so the split now separates the vals that were
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
  // The report records a constraint verdict only for its top #1; this candidate has no recorded constraint verdict.
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

  // GA-optimized indicator params for s10 (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-17-0315-s10_shuffle.md
  // training 0.784496 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference; the report records no constraint verdict for this candidate.
  // Historical net is higher than s10: 1073.54697 vs 768.63383.
  // searched 2023-07..2025-07: net=5646.46723, closed=862, forced=6, win=67.75%, exp=6.550426, PF=1.473, DD=0.47%, Sharpe=2.897
  // historical 2025-12..2026-06: net=1073.54697, closed=233, forced=2, win=63.52%, exp=4.607498, PF=1.299, DD=1.45%, Sharpe=2.764
  // Retained after user review on 2026-09-21.
  val s10_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 88, phase = 55, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 41,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 21, smoothingType = ValueTransformation.SMA(length = 43)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 11)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 9),
        upperBoundary = 66.0,
        lowerBoundary = 36.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 8))
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

  // SCGA-optimized params for the previous s1_v2_optimized (rules unchanged); retained research candidate.
  // Replaces the previous definition at the user's request on 2026-10-11; originally named s1_v2_optimized_v10.
  // The replaced definition and all batch comparisons are archived in docs/ga-promotions-2026-10-11.md.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // scga-optimisation-2026-10-10-0833-s1_v2_optimized_scga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 1.018380; absolute validation score 0.000000; first seen=143.
  // Fitness deltas vs the report's original target: search +0.109947; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net -931.63087 vs original target -403.22198; improvement -528.40889 vs H=120.00000; DD increase 0.33760909 pp;
  // winning months 1/4; fixed-trade stressed net -1049.54981; improvement -528.39134.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=10886.76522, closed=1384, forced=11, win=50.22%, exp=7.866160, PF=1.533, DD=0.79%, Sharpe=3.190,
  // gross=12248.63028, costs=1361.86506
  // historical 2025-12..2026-06: net=2383.96524, closed=404, forced=6, win=50.25%, exp=5.900904, PF=1.380, DD=1.28%, Sharpe=4.061,
  // gross=2790.59556, costs=406.63032
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s1_v2_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 16, phase = -6, power = 8),
        line2Transformation = ValueTransformation.JMA(length = 10, phase = 24, power = 10)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 90.0,
        lowerBoundary = 14.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 47)),
      Indicator.VolatilityRegimeDetection(atrLength = 32, smoothingType = ValueTransformation.SMA(length = 23))
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

  // SCGA-optimized indicator parameters (rules unchanged) for s1_v2_optimized (shuffle=true).
  // Highest all-fold search fitness from scga-optimisation-2026-10-24-2139-s1_v2_optimized_scga_explore.md.
  // Retained as s1_v2_optimized_v2 at the user's request on 2026-10-29; originally s1_v2_optimized_v19.
  // BASE RETAINED under trial-v1 (recorded research settings); the user selection does not change that decision.
  // Current/search-objective-v1: search 1.120312; absolute selection 0.000000; first seen generation 81.
  // Selection net improvement 198.7976450605812079175596621224587; required 120.0000.
  // Rejected: absolute-fitness, positive-net-profit, winning-months, worst-month, stressed-net-profit, absolute-constraint.
  // Full selection evidence and baseline comparisons: docs/ga-promotions-2026-10-29.md.
  // searched 2023-07..2025-07: net=10016.43542, closed=1496, forced=11, win=50.80%, exp=6.695478, PF=1.452, DD=0.98%, Sharpe=4.182
  // gross=11490.98656, costs=1474.55114; s1_v2_optimized net=10886.76522.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=2236.13410, closed=431, forced=6, win=49.42%, exp=5.188246, PF=1.338, DD=1.42%, Sharpe=1.959
  // gross=2667.21221, costs=431.07811; s1_v2_optimized net=2383.96524.
  val s1_v2_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 9, phase = -96, power = 7),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 45, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 94.0,
        lowerBoundary = 24.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 50)),
      Indicator.VolatilityRegimeDetection(atrLength = 15, smoothingType = ValueTransformation.SMA(length = 35))
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
  // Measured as a lineage reference. Survives 2023-07..2024-06 but holdout net (796) is dominated by s2_optimized (2713).
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

  // GA-optimized indicator params for the previous s4_optimized_v2 (rules unchanged; shuffle=true). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-0110-s4_optimized_v2_ga_explore.md
  // training 0.567081 -> validation 0.000000, retaining 0.0%; first searched generation 64.
  // Absent from final shortlist; validation is a diagnostic replay, not a selection verdict.
  // Retained alongside its base after user review on 2026-10-06; the other 40 batch additions were removed.
  // Replaces the previous s4_optimized_v2 at the user's request on 2026-10-11; originally named s4_optimized_v7.
  // The replaced definition is archived in docs/ga-promotions-2026-10-06.md; optimiser rounds use this replacement.
  // Retained research candidate, not a selected champion.
  // BREACHES 7 constraint(s) on validation data:
  //   - net profit is -149.2100338999267940774721635848769, required > 0
  //   - expectancy is -1.22303306, required > 0
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.7827856556808065176618196805890967, required >= 1.3
  //   - profit factor is 0.90155, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Validation fold vs target: net=+76.58405; closed=+1; forced=-1; costs=+0.99230; portfolio drawdown=-0.00 percentage points; constraint breaches=8 -> 7.
  // Candidate vs target, all-fold training fitness: +0.003745 (+0.66%).
  // Candidate vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Candidate vs strongest seed on training (s4_optimized_v1): +0.161473 (+39.81%).
  // Candidate vs strongest seed on validation (s4_optimized_v1): +0.000000 (n/a (baseline is zero)).
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=3645.35433, closed=611, forced=2, win=71.03%, exp=5.966210, PF=1.565, DD=0.37%, Sharpe=2.528
  // gross=4243.39033, costs=598.03600; previous base s4_optimized_v2 net=3708.07348.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1119.60856, closed=167, forced=3, win=71.86%, exp=6.704243, PF=1.666, DD=0.81%, Sharpe=2.015
  // gross=1284.47820, costs=164.86964; previous base s4_optimized_v2 net=990.17144.
  val s4_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 49, phase = -62, power = 1)),
      Indicator
        .KeltnerChannel(source = ValueSource.Close, middleBand = ValueTransformation.EMA(length = 28), atrLength = 21, atrMultiplier = 2.5),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 13),
        upperBoundary = 70.0,
        lowerBoundary = 33.0
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 28, smoothingType = ValueTransformation.SMA(length = 48))
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
  // higher than its in-sample PF. It kept its `_v2` suffix when that base was deleted; later candidates remain alongside it for comparison.
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

  // S13: CMF-confirmed Bollinger re-entry and trend-aligned squeeze breakout, exited at an RSX extreme.
  // Requires meaningful volume data. Rules inherited from the original s13; no price stop or time cap.
  // GA-optimized indicator params for the original s13 (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-25-0756-s13_ga_explore.md
  // training 0.956071 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 10 constraint(s) on validation data:
  //   - closed trades is 102, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - net profit is -175.2964907010058180098455654752162, required > 0
  //   - expectancy is -1.71859305, required > 0
  //   - median period profit is -144.089239441006290771291370327625, required > 0
  //   - profitable pair-months is 0.542, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.7688778092684613674372722966129509, required >= 1.3
  //   - profit factor is 0.88900, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Promoted from s13_optimized_v5 into s13 after user review on 2026-09-28.
  // Optimiser rounds now start from these parameters. The original s13 definition and history are archived
  // in docs/ga-promotions-2026-09-28.md. Selection reused the historical period; this is not independent validation.
  // Historical net is lower than the original s13: 655.02388 vs 685.30228.
  // Searched net moves the other way: 7077.95462 vs 5951.97849 for the original base.
  // searched 2023-07..2025-07: net=7077.95462, closed=602, forced=2, win=72.76%, exp=11.757400, PF=2.170, DD=0.49%, Sharpe=3.985
  // historical 2025-12..2026-06: net=655.02388, closed=152, forced=2, win=66.45%, exp=4.309368, PF=1.275, DD=1.38%, Sharpe=1.268
  val s13 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 79, phase = -5, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 36),
        stdDevLength = 36,
        stdDevMultiplier = 2.8
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = ValueTransformation.SMA(length = 61)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 8),
        upperBoundary = 68.0,
        lowerBoundary = 33.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.CMF(length = 20))
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

  // GA-optimized indicator parameters for s13 (shuffle=false).
  // Top #1 and highest all-fold search fitness from ga-optimisation-2026-10-20-0129-s13_ga_refine.md.
  // Replaces s13_optimized at the user's request on 2026-10-21; previously named s13_optimized_v12.
  // Diagnostic-only reference; the report retained its base under trial-v1 (recorded research settings).
  // The replaced definition and original batch names are archived in docs/ga-promotions-2026-10-20.md.
  // Current/search-objective-v1: search 1.127406; absolute selection 0.000000; first seen generation 129.
  // Selection net improvement -37.6239286868790467548260472139049; required 120.0000.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, worst-month, stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // searched 2023-07..2025-07: net=5617.24234, closed=765, forced=3, win=67.58%, exp=7.342800, PF=1.803, DD=0.47%, Sharpe=4.286
  // gross=6364.41989, costs=747.17755; s13 net=7216.75462.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1688.48296, closed=216, forced=2, win=71.76%, exp=7.817051, PF=1.825, DD=1.64%, Sharpe=2.614
  // gross=1904.01235, costs=215.52939; s13 net=578.42388.
  val s13_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 76, phase = -30, power = 2)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 38),
        stdDevLength = 43,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 27, smoothingType = ValueTransformation.SMA(length = 37)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 63.0,
        lowerBoundary = 29.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.CMF(length = 25))
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
  // historical period (net -79, PF 0.976). The former s12 variants also lost there; the replacement s12_optimized selected on
  // 2026-10-29 nets 836.86 on that reused period. See docs/ga-promotions-2026-10-29.md for the searched-year disagreement and selection rejection.
  // Included in BatchBacktester and scheduled for renewed optimisation at the user's request on 2026-10-21.
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

  // GA-optimized indicator and rule parameters for s12 (shuffle=true).
  // Highest all-fold search fitness from ga-optimisation-2026-10-24-2231-s12_ga_explore.md.
  // Retained as s12_optimized at the user's request on 2026-10-29; originally s12_optimized_v4.
  // BASE RETAINED under trial-v1 (recorded research settings); the user selection does not change that decision.
  // Current/search-objective-v1: search 0.216153; absolute selection 0.000000; first seen generation 38.
  // Selection net improvement 953.0270292146365086925192740122005; required 120.0000.
  // Rejected: absolute-fitness, positive-net-profit, winning-months, worst-month, stressed-net-profit, absolute-constraint.
  // Replaces the legacy s12_optimized; its definition is archived in docs/ga-promotions-2026-10-29.md.
  // Included in batch evaluation and its own optimisation rounds alongside s12.
  // Full selection evidence and baseline comparisons: docs/ga-promotions-2026-10-29.md.
  // searched 2023-07..2025-07: net=4043.93055, closed=682, forced=2, win=40.47%, exp=5.929517, PF=1.445, DD=1.44%, Sharpe=1.459
  // gross=4712.20275, costs=668.27220; s12 net=3291.74961.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=836.86488, closed=184, forced=4, win=42.39%, exp=4.548179, PF=1.269, DD=1.13%, Sharpe=1.254
  // gross=1021.45228, costs=184.58740; s12 net=-78.88478.
  val s12_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.IchimokuKijunSen(length = 11)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.CMF(length = 13),
        upperBoundary = 0.32,
        lowerBoundary = -0.02
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 23, smoothingType = ValueTransformation.SMA(length = 57))
    ),
    rules = TradeStrategy(
      openRules = List(
        Rule(
          action = TradeAction.OpenLong,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsUpward,
            Rule.Condition.TrendActiveFor(4.hours),
            Rule.Condition.momentumEnteredOverbought, // CMF crossed above 0.32
            Rule.Condition.volatilityIsLow
          )
        ),
        Rule(
          action = TradeAction.OpenShort,
          conditions = Rule.Condition.allOf(
            Rule.Condition.NoPosition,
            Rule.Condition.trendIsDownward,
            Rule.Condition.TrendActiveFor(4.hours),
            Rule.Condition.momentumEnteredOversold, // CMF crossed below -0.02
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

  // GA-optimized indicator params for s4_optimized_v1 (rules unchanged). Best by validation from ga-optimisation-2026-09-02-2305-s4_optimized_v1_shuffle.md (NOTHING SELECTED) (training 0.500224 -> validation 0.000000, retaining n/a, shuffled GA).
  // NOTHING SELECTED: no finalist scored above zero on validation data.
  // Measured as a lineage reference. Historical net (637) is lower than the replacement s4_optimized_v2 (1120, formerly v7).
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

  // GA-optimized indicator params for the previous s6_optimized (rules unchanged). Champion from
  // ga-optimisation-2026-09-18-0319-s6_optimized.md
  // training 0.667709 -> validation 0.192699, retaining 28.9%, unshuffled GA.
  // Final shortlist rank 1; training rank 12.
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.542, required >= 0.550
  //   - most concentrated pair's best month is 0.772, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.11589, required >= 1.2
  // Promoted from s6_optimized_v3 into s6_optimized after user review on 2026-09-21.
  // The previous definition is archived in docs/ga-promotions-2026-09-21.md.
  // Historical net is higher than the previous s6_optimized: 1000.26582 vs 663.08878.
  // Searched net moves the other way: 5787.60378 vs 6242.07098 for the previous base.
  // searched 2023-07..2025-07: net=5787.60378, closed=818, forced=5, win=68.58%, exp=7.075310, PF=1.681, DD=0.72%, Sharpe=3.463
  // historical 2025-12..2026-06: net=1000.26582, closed=227, forced=1, win=68.28%, exp=4.406457, PF=1.356, DD=2.42%, Sharpe=1.374
  val s6_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 76, phase = -15, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 36),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 22, smoothingType = ValueTransformation.SMA(length = 75)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 64.0,
        lowerBoundary = 32.0
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

  // GA-optimized indicator parameters (rules unchanged) for s2_optimized (shuffle=false).
  // Top #1 and highest all-fold search fitness from ga-optimisation-2026-10-24-1006-s2_optimized_ga_refine.md.
  // Retained as s2_optimized_v3 at the user's request on 2026-10-29; originally s2_optimized_v19.
  // BASE RETAINED under trial-v1 (recorded research settings); the user selection does not change that decision.
  // Current/search-objective-v1: search 1.152967; absolute selection 0.000000; first seen generation 80.
  // Selection net improvement 19.9696897746375114375305466215748; required 120.0000.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, winning-months, stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Full selection evidence and baseline comparisons: docs/ga-promotions-2026-10-29.md.
  // searched 2023-07..2025-07: net=12040.53530, closed=1656, forced=12, win=40.76%, exp=7.270855, PF=1.541, DD=0.90%, Sharpe=2.725
  // gross=13684.07450, costs=1643.53920; s2_optimized net=12225.94093.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=2620.43600, closed=445, forced=6, win=40.23%, exp=5.888620, PF=1.385, DD=1.25%, Sharpe=2.802
  // gross=3050.07467, costs=429.63867; s2_optimized net=2713.33834.
  val s2_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 15, phase = 59, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 22, phase = -17, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 24.0
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 37, smoothingType = ValueTransformation.SMA(length = 40))
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
