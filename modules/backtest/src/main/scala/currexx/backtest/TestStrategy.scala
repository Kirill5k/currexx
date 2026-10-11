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
  * The 2026-09-21 batch measured 34 distinct candidates from the 20 reports dated 2026-09-16..18. After user review, s1_v2_optimized_v7,
  * s6_optimized_v3 and s4_optimized_v6 were promoted into s1_v2_optimized, s6_optimized and s4_optimized_v2, replacing the previous
  * definitions. The retained s10_optimized_v2 was subsequently renamed s10_optimized. The other 30 additions were deleted. All four remain
  * in `BatchBacktester`; full measurements, original names and replaced definitions are archived in docs/ga-promotions-2026-09-21.md.
  * Selection used the already-viewed historical period and does not establish independent validation.
  *
  * The 2026-09-28 batch measured 29 distinct candidates from the 25 reports dated 2026-09-22..25. After user review, s1_v2_optimized_v4 and
  * s13_optimized remain alongside their bases, and s13_optimized_v5 was promoted into s13, replacing its original definition. The other 26
  * additions were deleted. All three selected definitions remain in `BatchBacktester`; measurements, final decisions and the original s13
  * definition are archived in docs/ga-promotions-2026-09-28.md.
  *
  * The 2026-10-06 batch measured 41 distinct candidates from the 30 reports dated 2026-10-04..06. After user review, only s4_optimized_v7
  * remains alongside its base; the other 40 additions were deleted. The retained candidate was the highest all-fold fitness reference from
  * its report, not a selected champion. See docs/ga-promotions-2026-10-06.md for all measurements and the final decision, including the
  * deleted candidates.
  *
  * The 2026-10-11 batch adds 36 distinct diagnostic references from 44 reports dated 2026-10-09..10: final rank 1 plus the highest recorded
  * all-fold fitness candidate when distinct. Every report retained its base under trial-v1; these references do not establish approved
  * upgrades. See docs/ga-promotions-2026-10-11.md for report mappings, rejections and continuous measurements.
  *
  * Register every public strategy val in `StrategyCatalogue`, which controls batch membership and order. Its completeness test catches
  * omitted definitions. A val excluded from batch runs carries a `Not in BatchBacktester` line saying which val dominates it; these lineage
  * entries remain in the catalogue so a report filename still resolves to the thing it selected.
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

  // SCGA-optimized params for s10_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // scga-optimisation-2026-10-10-1701-s10_v2_scga_explore.md (final rank 1 + highest all-fold fitness; shuffle=true).
  // All-fold search score 1.035564; absolute validation score 0.000000; first seen=146.
  // Fitness deltas vs target: search +0.743459; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -889.82136 vs H=120.00000; DD increase 1.08464967 pp;
  // winning months 1/4; fixed-trade stressed net -1243.23903; improvement -915.68365.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6148.24722, closed=804, forced=3, win=76.62%, exp=7.647074, PF=1.878, DD=0.30%, Sharpe=3.744,
  // gross=6933.29375, costs=785.04653
  // historical 2025-12..2026-06: net=214.71757, closed=223, forced=3, win=65.92%, exp=0.962859, PF=1.073, DD=0.71%, Sharpe=0.856,
  // gross=433.73695, costs=219.01938
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s10_v2_optimized_v7 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 89, phase = -81, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 94),
        stdDevLength = 43,
        stdDevMultiplier = 3.0
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 10, smoothingType = ValueTransformation.SMA(length = 7)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 13)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 73.0,
        lowerBoundary = 33.0
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

  // SCGA-optimized params for s10_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // scga-optimisation-2026-10-10-0444-s10_v2_scga_explore.md (final rank 1 + highest all-fold fitness; shuffle=true).
  // All-fold search score 0.998464; absolute validation score 0.000000; first seen=114.
  // Fitness deltas vs target: search +0.706359; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -827.49971 vs H=120.00000; DD increase 1.04168256 pp;
  // winning months 0/4; fixed-trade stressed net -1193.25240; improvement -865.69701.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6459.18285, closed=802, forced=2, win=74.69%, exp=8.053844, PF=1.859, DD=0.27%, Sharpe=4.602,
  // gross=7237.42464, costs=778.24180
  // historical 2025-12..2026-06: net=187.32271, closed=252, forced=5, win=67.06%, exp=0.743344, PF=1.060, DD=0.76%, Sharpe=0.742,
  // gross=435.12719, costs=247.80447
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s10_v2_optimized_v8 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 75, phase = -22, power = 2)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 90),
        stdDevLength = 48,
        stdDevMultiplier = 2.8
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 14, smoothingType = ValueTransformation.SMA(length = 13)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 50)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 73.0,
        lowerBoundary = 31.0
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

  // GA-optimized params for s10_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-0405-s10_v2_ga_explore.md (final rank 1 + highest all-fold fitness; shuffle=true).
  // All-fold search score 0.967529; absolute validation score 0.000000; first seen=93.
  // Fitness deltas vs target: search +0.675424; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -1095.96883 vs H=120.00000; DD increase 1.69533972 pp;
  // winning months 0/4; fixed-trade stressed net -1434.37726; improvement -1106.82187.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6268.12732, closed=645, forced=6, win=68.99%, exp=9.718027, PF=1.610, DD=0.42%, Sharpe=3.373,
  // gross=6895.36498, costs=627.23766
  // historical 2025-12..2026-06: net=401.18644, closed=186, forced=0, win=59.14%, exp=2.156916, PF=1.116, DD=0.83%, Sharpe=1.148,
  // gross=587.19615, costs=186.00971
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s10_v2_optimized_v9 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 89, phase = 1, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 29),
        stdDevLength = 36,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 22, smoothingType = ValueTransformation.SMA(length = 50)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 8)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 68.0,
        lowerBoundary = 28.0
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

  // GA-optimized params for s10_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-1622-s10_v2_ga_explore.md (final rank 1 + highest all-fold fitness; shuffle=true).
  // All-fold search score 0.945139; absolute validation score 0.000000; first seen=98.
  // Fitness deltas vs target: search +0.653034; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 0.00567 vs H=120.00000; DD increase 0.08202994 pp;
  // winning months 2/4; fixed-trade stressed net -347.17603; improvement -19.62064.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, winning-months, worst-month, stressed-net-profit,
  // stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=4689.71901, closed=644, forced=1, win=70.81%, exp=7.282172, PF=1.828, DD=0.33%, Sharpe=4.740,
  // gross=5315.64928, costs=625.93027
  // historical 2025-12..2026-06: net=661.31068, closed=179, forced=1, win=65.92%, exp=3.694473, PF=1.371, DD=0.68%, Sharpe=2.228,
  // gross=840.11810, costs=178.80742
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s10_v2_optimized_v10 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 100, phase = -6, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 32),
        stdDevLength = 37,
        stdDevMultiplier = 2.5
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = ValueTransformation.SMA(length = 45)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 12)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 5),
        upperBoundary = 69.0,
        lowerBoundary = 27.0
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

  // GA-optimized params for s10_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-1543-s10_v2_ga_refine.md (final rank 1 + highest all-fold fitness; shuffle=false).
  // All-fold search score 0.872565; absolute validation score 0.000000; first seen=139.
  // Fitness deltas vs target: search +0.580460; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -1105.32393 vs H=120.00000; DD increase 1.75871699 pp;
  // winning months 1/4; fixed-trade stressed net -1451.67757; improvement -1124.12219.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6646.03981, closed=714, forced=6, win=69.19%, exp=9.308179, PF=1.595, DD=0.53%, Sharpe=3.339,
  // gross=7344.75353, costs=698.71372
  // historical 2025-12..2026-06: net=781.86442, closed=208, forced=0, win=62.02%, exp=3.758964, PF=1.208, DD=0.98%, Sharpe=1.762,
  // gross=990.20212, costs=208.33770
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s10_v2_optimized_v11 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 87, phase = -6, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 32),
        stdDevLength = 34,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 29, smoothingType = ValueTransformation.SMA(length = 51)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 7)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 68.0,
        lowerBoundary = 28.0
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

  // GA-optimized params for s10_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-0325-s10_v2_ga_refine.md (final rank 1 + highest all-fold fitness; shuffle=false).
  // All-fold search score 0.788517; absolute validation score 0.000000; first seen=77.
  // Fitness deltas vs target: search +0.496412; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -1152.54450 vs H=120.00000; DD increase 1.79163899 pp;
  // winning months 0/4; fixed-trade stressed net -1491.85261; improvement -1164.29722.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6169.04813, closed=630, forced=4, win=69.05%, exp=9.792140, PF=1.600, DD=0.43%, Sharpe=3.336,
  // gross=6783.18624, costs=614.13811
  // historical 2025-12..2026-06: net=781.68620, closed=182, forced=0, win=62.64%, exp=4.294979, PF=1.239, DD=1.38%, Sharpe=2.253,
  // gross=963.45568, costs=181.76948
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s10_v2_optimized_v12 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 81, phase = 34, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 29),
        stdDevLength = 37,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 28, smoothingType = ValueTransformation.SMA(length = 35)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 9)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 68.0,
        lowerBoundary = 28.0
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

  // GA-optimized params for s10_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-1852-s10_optimized_ga_explore.md (final rank 1 + highest all-fold fitness; shuffle=true).
  // All-fold search score 0.808293; absolute validation score 0.000000; first seen=123.
  // ga-optimisation-2026-10-09-2308-s10_optimized_ga_explore.md (final rank 1 + highest all-fold fitness; shuffle=true).
  // All-fold search score 0.808293; absolute validation score 0.000000; first seen=12.
  // Fitness deltas vs target: search +0.008163; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 0.00000 vs H=120.00000; DD increase 0.00000000 pp;
  // winning months 0/4; fixed-trade stressed net -376.39405; improvement 0.00000.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, winning-months, stressed-net-profit, stressed-net-improvement,
  // absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=5636.96723, closed=864, forced=6, win=67.82%, exp=6.524268, PF=1.471, DD=0.47%, Sharpe=2.872,
  // gross=6477.89034, costs=840.92311
  // historical 2025-12..2026-06: net=1100.94697, closed=233, forced=2, win=63.95%, exp=4.725094, PF=1.313, DD=1.45%, Sharpe=2.730,
  // gross=1335.85802, costs=234.91106
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s10_optimized_v8 = TestStrategy(
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
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 18))
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

  // GA-optimized params for s10_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-1808-s10_optimized_ga_refine.md (final rank 1; shuffle=false).
  // All-fold search score 0.804195; absolute validation score 0.000000; first seen=87.
  // Fitness deltas vs target: search +0.004065; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 257.98667 vs H=120.00000; DD increase -0.01570905 pp;
  // winning months 3/4; fixed-trade stressed net -119.03588; improvement 257.35817.
  // Rejected: absolute-fitness, positive-net-profit, stressed-net-profit, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=5663.45735, closed=864, forced=6, win=67.94%, exp=6.554927, PF=1.474, DD=0.53%, Sharpe=2.859,
  // gross=6504.48249, costs=841.02513
  // historical 2025-12..2026-06: net=1098.37216, closed=235, forced=2, win=64.26%, exp=4.673924, PF=1.306, DD=1.43%, Sharpe=3.108,
  // gross=1335.55096, costs=237.17880
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s10_optimized_v9 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 86, phase = 67, power = 1)),
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

  // GA-optimized params for s10_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-1808-s10_optimized_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 0.804195; absolute validation score 0.000000; first seen=73.
  // Fitness deltas vs target: search +0.004065; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 257.98667 vs H=120.00000; DD increase -0.01570905 pp;
  // winning months 3/4; fixed-trade stressed net -119.03588; improvement 257.35817.
  // Rejected: absolute-fitness, positive-net-profit, stressed-net-profit, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=5663.45735, closed=864, forced=6, win=67.94%, exp=6.554927, PF=1.474, DD=0.53%, Sharpe=2.859,
  // gross=6504.48249, costs=841.02513
  // historical 2025-12..2026-06: net=1098.37216, closed=235, forced=2, win=64.26%, exp=4.673924, PF=1.306, DD=1.43%, Sharpe=3.108,
  // gross=1335.55096, costs=237.17880
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s10_optimized_v10 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 86, phase = 67, power = 1)),
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
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 6))
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

  // GA-optimized indicator params for the previous s1_v2_optimized (rules unchanged). Training fitness leader from
  // scga-optimisation-2026-09-18-2227-s1_v2_optimized_shuffle.md
  // training 0.908433 -> validation 0.000000, retaining 0.0%, shuffled SCGA.
  // Final shortlist rank 3; training rank 1.
  // The report records a constraint verdict only for its top #1; this candidate has no recorded constraint verdict.
  // Promoted from s1_v2_optimized_v7 into s1_v2_optimized after user review on 2026-09-21.
  // The previous definition is archived in docs/ga-promotions-2026-09-21.md.
  // Historical net is lower than the previous s1_v2_optimized: 1443.13093 vs 1535.84151.
  // Searched net moves the other way: 9094.96794 vs 6285.64646 for the previous base.
  // searched 2023-07..2025-07: net=9094.96794, closed=1403, forced=11, win=49.75%, exp=6.482515, PF=1.441, DD=0.85%, Sharpe=2.866
  // historical 2025-12..2026-06: net=1443.13093, closed=379, forced=6, win=50.92%, exp=3.807733, PF=1.222, DD=1.87%, Sharpe=2.256
  val s1_v2_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 13, phase = 36, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 61, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 15),
        upperBoundary = 87.0,
        lowerBoundary = 27.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 47)),
      Indicator.VolatilityRegimeDetection(atrLength = 24, smoothingType = ValueTransformation.SMA(length = 32))
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged). Top #1 from
  // ga-optimisation-2026-09-25-1701-s1_v2_optimized_ga_refine.md
  // training 0.644767 -> validation 0.013205, retaining 2.0%, unshuffled GA.
  // Final shortlist rank 1; training rank 22.
  // BREACHES 5 constraint(s) on validation data:
  //   - profitable pair-months is 0.417, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 1.144293411806394303310986058285837, required >= 1.3
  //   - profit factor is 1.05799, required >= 1.2
  //   - costs as a share of gross profit is 0.496, required <= 0.400
  // Retained alongside s1_v2_optimized after user review on 2026-09-28; see docs/ga-promotions-2026-09-28.md.
  // Historical net is higher than s1_v2_optimized: 2063.10747 vs 1443.13093.
  // Searched net moves the other way: 7780.58696 vs 9094.96794 for the base.
  // searched 2023-07..2025-07: net=7780.58696, closed=1191, forced=11, win=49.12%, exp=6.532819, PF=1.373, DD=0.83%, Sharpe=2.664
  // historical 2025-12..2026-06: net=2063.10747, closed=320, forced=6, win=49.69%, exp=6.447211, PF=1.334, DD=1.49%, Sharpe=2.832
  val s1_v2_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 32, power = 5),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 80, power = 9)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16),
        upperBoundary = 89.0,
        lowerBoundary = 25.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 48)),
      Indicator.VolatilityRegimeDetection(atrLength = 23, smoothingType = ValueTransformation.SMA(length = 22))
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

  // GA-optimized params for s1_v2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-0722-s1_v2_optimized_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 1.046911; absolute validation score 0.000000; first seen=147.
  // Fitness deltas vs target: search +0.138478; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -437.01361 vs H=120.00000; DD increase 0.14627504 pp;
  // winning months 1/4; fixed-trade stressed net -957.99602; improvement -436.83756.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=10324.11031, closed=1391, forced=11, win=48.81%, exp=7.422078, PF=1.496, DD=0.72%, Sharpe=2.876,
  // gross=11689.70816, costs=1365.59785
  // historical 2025-12..2026-06: net=1986.19327, closed=382, forced=6, win=50.26%, exp=5.199459, PF=1.319, DD=1.10%, Sharpe=4.203,
  // gross=2366.75432, costs=380.56105
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s1_v2_optimized_v9 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 34, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 5, phase = 90, power = 9)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 81.0,
        lowerBoundary = 23.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 48)),
      Indicator.VolatilityRegimeDetection(atrLength = 24, smoothingType = ValueTransformation.SMA(length = 32))
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

  // SCGA-optimized params for s1_v2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // scga-optimisation-2026-10-10-0833-s1_v2_optimized_scga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 1.018380; absolute validation score 0.000000; first seen=143.
  // Fitness deltas vs target: search +0.109947; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -528.40889 vs H=120.00000; DD increase 0.33760909 pp;
  // winning months 1/4; fixed-trade stressed net -1049.54981; improvement -528.39134.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=10886.76522, closed=1384, forced=11, win=50.22%, exp=7.866160, PF=1.533, DD=0.79%, Sharpe=3.190,
  // gross=12248.63028, costs=1361.86506
  // historical 2025-12..2026-06: net=2383.96524, closed=404, forced=6, win=50.25%, exp=5.900904, PF=1.380, DD=1.28%, Sharpe=4.061,
  // gross=2790.59556, costs=406.63032
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s1_v2_optimized_v10 = TestStrategy(
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

  // GA-optimized params for s1_v2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-0758-s1_v2_optimized_ga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 0.981216; absolute validation score 0.000000; first seen=45.
  // Fitness deltas vs target: search +0.072783; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -287.30836 vs H=120.00000; DD increase 0.16504197 pp;
  // winning months 0/4; fixed-trade stressed net -813.19202; improvement -292.03355.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=9571.54504, closed=1453, forced=11, win=50.65%, exp=6.587436, PF=1.451, DD=0.85%, Sharpe=3.047,
  // gross=11000.14337, costs=1428.59833
  // historical 2025-12..2026-06: net=1408.59261, closed=394, forced=6, win=51.52%, exp=3.575108, PF=1.206, DD=1.62%, Sharpe=2.627,
  // gross=1802.26501, costs=393.67241
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s1_v2_optimized_v11 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 13, phase = 35, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 49, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 13),
        upperBoundary = 84.0,
        lowerBoundary = 25.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 48)),
      Indicator.VolatilityRegimeDetection(atrLength = 24, smoothingType = ValueTransformation.SMA(length = 32))
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

  // GA-optimized params for s1_v2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-1942-s1_v2_optimized_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 0.970874; absolute validation score 0.000000; first seen=105.
  // Fitness deltas vs target: search +0.062441; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -313.67429 vs H=120.00000; DD increase 0.17859243 pp;
  // winning months 0/4; fixed-trade stressed net -840.91378; improvement -319.75531.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=9270.79035, closed=1463, forced=11, win=50.79%, exp=6.336836, PF=1.436, DD=0.85%, Sharpe=3.209,
  // gross=10709.71850, costs=1438.92815
  // historical 2025-12..2026-06: net=1175.85002, closed=395, forced=6, win=52.41%, exp=2.976836, PF=1.175, DD=1.65%, Sharpe=2.497,
  // gross=1569.98882, costs=394.13879
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s1_v2_optimized_v12 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 13, phase = 36, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 62, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 80.0,
        lowerBoundary = 26.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 48)),
      Indicator.VolatilityRegimeDetection(atrLength = 24, smoothingType = ValueTransformation.SMA(length = 32))
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

  // SCGA-optimized params for s1_v2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // scga-optimisation-2026-10-10-2054-s1_v2_optimized_scga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 0.962602; absolute validation score 0.000000; first seen=106.
  // Fitness deltas vs target: search +0.054169; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -145.80086 vs H=120.00000; DD increase -0.10765183 pp;
  // winning months 1/4; fixed-trade stressed net -665.53337; improvement -144.37491.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, winning-months, worst-month, stressed-net-profit,
  // stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=11003.14187, closed=1331, forced=11, win=49.36%, exp=8.266823, PF=1.540, DD=0.74%, Sharpe=3.006,
  // gross=12308.73904, costs=1305.59717
  // historical 2025-12..2026-06: net=1731.42100, closed=377, forced=6, win=49.60%, exp=4.592629, PF=1.265, DD=1.15%, Sharpe=3.541,
  // gross=2110.23859, costs=378.81759
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s1_v2_optimized_v13 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 26, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 82, power = 9)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 13),
        upperBoundary = 83.0,
        lowerBoundary = 22.0
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

  // GA-optimized params for s1_v2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-2018-s1_v2_optimized_ga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 0.954192; absolute validation score 0.000000; first seen=34.
  // Fitness deltas vs target: search +0.045759; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -306.76983 vs H=120.00000; DD increase 0.19705761 pp;
  // winning months 0/4; fixed-trade stressed net -832.65380; improvement -311.49533.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=9330.02517, closed=1452, forced=11, win=50.62%, exp=6.425637, PF=1.439, DD=0.85%, Sharpe=3.080,
  // gross=10757.63334, costs=1427.60817
  // historical 2025-12..2026-06: net=1310.07910, closed=390, forced=6, win=51.28%, exp=3.359177, PF=1.191, DD=1.62%, Sharpe=2.475,
  // gross=1700.02821, costs=389.94911
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s1_v2_optimized_v14 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 13, phase = 35, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 49, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 83.0,
        lowerBoundary = 26.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 48)),
      Indicator.VolatilityRegimeDetection(atrLength = 24, smoothingType = ValueTransformation.SMA(length = 32))
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

  // GA-optimized params for s2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-1736-s2_optimized_ga_explore.md (final rank 1; shuffle=true).
  // All-fold search score 1.017807; absolute validation score 0.011237; first seen=66.
  // ga-optimisation-2026-10-09-2155-s2_optimized_ga_explore.md (final rank 1; shuffle=true).
  // All-fold search score 1.017807; absolute validation score 0.011237; first seen=100.
  // Fitness deltas vs target: search +0.071916; validation +0.011237 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 448.30466 vs H=120.00000; DD increase -0.58951948 pp;
  // winning months 3/4; fixed-trade stressed net 81.19490; improvement 432.30574.
  // Rejected: worst-month, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=10547.66867, closed=1727, forced=12, win=39.84%, exp=6.107509, PF=1.453, DD=0.88%, Sharpe=3.218,
  // gross=12248.46780, costs=1700.79913
  // historical 2025-12..2026-06: net=2450.87374, closed=507, forced=6, win=38.86%, exp=4.834071, PF=1.337, DD=1.31%, Sharpe=3.322,
  // gross=2950.07641, costs=499.20267
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s2_optimized_v9 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 88, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 22, phase = -19, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 28.0
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

  // GA-optimized params for s2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-2155-s2_optimized_ga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 1.030974; absolute validation score 0.010089; first seen=17.
  // Fitness deltas vs target: search +0.085083; validation +0.010089 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 429.36260 vs H=120.00000; DD increase -0.50034009 pp;
  // winning months 3/4; fixed-trade stressed net 62.25355; improvement 413.36439.
  // Rejected: worst-month, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=10271.37878, closed=1747, forced=12, win=39.84%, exp=5.879438, PF=1.434, DD=0.88%, Sharpe=3.135,
  // gross=11992.55698, costs=1721.17820
  // historical 2025-12..2026-06: net=2540.27922, closed=511, forced=6, win=38.94%, exp=4.971192, PF=1.351, DD=1.31%, Sharpe=3.485,
  // gross=3043.46465, costs=503.18542
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s2_optimized_v10 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 96, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 22, phase = -17, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 28.0
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

  // GA-optimized params for s2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-0943-s2_optimized_ga_refine.md (final rank 1; shuffle=false).
  // All-fold search score 1.020013; absolute validation score 0.009245; first seen=76.
  // ga-optimisation-2026-10-10-1015-s2_optimized_ga_explore.md (final rank 1; shuffle=true).
  // All-fold search score 1.020013; absolute validation score 0.009245; first seen=87.
  // Fitness deltas vs target: search +0.074122; validation +0.009245 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 433.90466 vs H=120.00000; DD increase -0.57480081 pp;
  // winning months 3/4; fixed-trade stressed net 66.79490; improvement 417.90574.
  // Rejected: worst-month, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=10585.99817, closed=1727, forced=12, win=39.95%, exp=6.129704, PF=1.455, DD=0.88%, Sharpe=3.228,
  // gross=12286.06813, costs=1700.06996
  // historical 2025-12..2026-06: net=2456.77374, closed=507, forced=6, win=38.86%, exp=4.845708, PF=1.338, DD=1.31%, Sharpe=3.334,
  // gross=2955.97641, costs=499.20267
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s2_optimized_v11 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 90, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 22, phase = -19, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 28.0
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

  // GA-optimized params for s2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-1702-s2_optimized_ga_refine.md (final rank 1; shuffle=false).
  // All-fold search score 1.030729; absolute validation score 0.005920; first seen=28.
  // ga-optimisation-2026-10-09-2121-s2_optimized_ga_refine.md (final rank 1; shuffle=false).
  // All-fold search score 1.030729; absolute validation score 0.005920; first seen=75.
  // Fitness deltas vs target: search +0.084838; validation +0.005920 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 378.90973 vs H=120.00000; DD increase -0.48039109 pp;
  // winning months 3/4; fixed-trade stressed net 11.80113; improvement 362.91196.
  // Rejected: worst-month, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=10976.92510, closed=1732, forced=12, win=40.01%, exp=6.337717, PF=1.474, DD=0.87%, Sharpe=3.334,
  // gross=12683.82278, costs=1706.89768
  // historical 2025-12..2026-06: net=2321.52194, closed=503, forced=6, win=38.97%, exp=4.615352, PF=1.317, DD=1.32%, Sharpe=2.998,
  // gross=2815.57653, costs=494.05459
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s2_optimized_v12 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 55, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 22, phase = -18, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 28.0
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

  // GA-optimized params for s2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-1702-s2_optimized_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 1.055238; absolute validation score 0.004574; first seen=3.
  // ga-optimisation-2026-10-09-2121-s2_optimized_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 1.055238; absolute validation score 0.004574; first seen=85.
  // Fitness deltas vs target: search +0.109347; validation +0.004574 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 365.10973 vs H=120.00000; DD increase -0.48039109 pp;
  // winning months 3/4; fixed-trade stressed net -1.99887; improvement 349.11196.
  // Rejected: worst-month, stressed-net-profit, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=10766.93669, closed=1728, forced=12, win=39.93%, exp=6.230866, PF=1.462, DD=0.88%, Sharpe=3.229,
  // gross=12469.41902, costs=1702.48232
  // historical 2025-12..2026-06: net=2538.09428, closed=507, forced=6, win=38.86%, exp=5.006103, PF=1.355, DD=1.31%, Sharpe=3.201,
  // gross=3035.58928, costs=497.49500
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s2_optimized_v13 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 61, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 22, phase = -18, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 28.0
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

  // GA-optimized params for s2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-1015-s2_optimized_ga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 1.038802; absolute validation score 0.000000; first seen=67.
  // Fitness deltas vs target: search +0.092911; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 22.73340 vs H=120.00000; DD increase -0.49188171 pp;
  // winning months 1/4; fixed-trade stressed net -346.62062; improvement 4.49021.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, winning-months, worst-month, stressed-net-profit,
  // stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=9407.31032, closed=1726, forced=12, win=38.18%, exp=5.450354, PF=1.395, DD=1.12%, Sharpe=2.694,
  // gross=11116.02240, costs=1708.71208
  // historical 2025-12..2026-06: net=2176.68350, closed=498, forced=6, win=38.55%, exp=4.370850, PF=1.293, DD=1.37%, Sharpe=3.650,
  // gross=2666.58573, costs=489.90223
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s2_optimized_v14 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 12, phase = 80, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 26, phase = -17, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 27.0
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

  // GA-optimized params for s2_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-1736-s2_optimized_ga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 1.024412; absolute validation score 0.000000; first seen=29.
  // ga-optimisation-2026-10-10-0943-s2_optimized_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 1.024412; absolute validation score 0.000000; first seen=25.
  // Fitness deltas vs target: search +0.078521; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 16.82169 vs H=120.00000; DD increase -0.11339556 pp;
  // winning months 2/4; fixed-trade stressed net -334.29030; improvement 16.82053.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, winning-months, stressed-net-profit, stressed-net-improvement,
  // absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=12072.59152, closed=1718, forced=12, win=40.11%, exp=7.027120, PF=1.539, DD=0.88%, Sharpe=2.837,
  // gross=13776.69820, costs=1704.10668
  // historical 2025-12..2026-06: net=2599.01861, closed=468, forced=6, win=39.10%, exp=5.553459, PF=1.374, DD=1.31%, Sharpe=3.315,
  // gross=3049.24293, costs=450.22431
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s2_optimized_v15 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 90, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 22, phase = -19, power = 1)
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

  // GA-optimized indicator params for the original s4_optimized_v2 (rules unchanged). Training fitness leader from
  // ga-optimisation-2026-09-18-0202-s4_optimized_v2_shuffle.md
  // training 0.563336 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 3; training rank 1.
  // The report records a constraint verdict only for its top #1; this candidate has no recorded constraint verdict.
  // Promoted from s4_optimized_v6 into s4_optimized_v2 after user review on 2026-09-21.
  // Both s4 Optimiser rounds now start from these parameters; the original definition is archived in the promotion report.
  // Historical PF is 1.568 vs 1.565 for the original base, DD 1.01% vs 0.73%, and Sharpe 1.641 vs 2.157.
  // Historical net is lower than the original s4_optimized_v2: 990.17144 vs 1079.89722.
  // Searched net moves the other way: 3708.07348 vs 1281.32473 for the original base.
  // searched 2023-07..2025-07: net=3708.07348, closed=604, forced=2, win=71.03%, exp=6.139195, PF=1.590, DD=0.37%, Sharpe=2.637
  // historical 2025-12..2026-06: net=990.17144, closed=166, forced=2, win=71.69%, exp=5.964888, PF=1.568, DD=1.01%, Sharpe=1.641
  val s4_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 50, phase = -50, power = 1)),
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

  // GA-optimized indicator params for the original s13 (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-24-2237-s13_ga_refine.md
  // training 0.958700 -> validation 0.000000, retaining 0.0%, unshuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 7 constraint(s) on validation data:
  //   - net profit is -78.57097120008248452590180465887725, required > 0
  //   - expectancy is -0.61383571, required > 0
  //   - median period profit is -68.58514996604900739423757831044, required > 0
  //   - most concentrated pair's best month is 0.877, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.8946694362885034669148294692629228, required >= 1.3
  //   - profit factor is 0.95243, required >= 1.2
  //   - costs as a share of gross profit is 2.598, required <= 0.400
  // Retained alongside the replacement s13 after user review on 2026-09-28; see docs/ga-promotions-2026-09-28.md.
  // Historical net is higher than the original s13: 1625.26462 vs 685.30228.
  // Searched net moves the other way: 5588.05807 vs 5951.97849 for the original base.
  // Historical net also exceeds the replacement s13: 1625.26462 vs 655.02388; searched net is lower (5588.05807 vs 7077.95462).
  // searched 2023-07..2025-07: net=5588.05807, closed=751, forced=3, win=66.71%, exp=7.440823, PF=1.771, DD=0.43%, Sharpe=3.928
  // historical 2025-12..2026-06: net=1625.26462, closed=214, forced=2, win=70.09%, exp=7.594694, PF=1.734, DD=1.65%, Sharpe=2.343
  val s13_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 78, phase = 5, power = 2)),
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
        lowerBoundary = 28.0
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

  // GA-optimized params for s13 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-1740-s13_ga_refine.md (final rank 1 + highest all-fold fitness; shuffle=false).
  // All-fold search score 0.989880; absolute validation score 0.000000; first seen=104.
  // Fitness deltas vs target: search +0.020937; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -209.82441 vs H=120.00000; DD increase -0.60356899 pp;
  // winning months 2/4; fixed-trade stressed net -441.36645; improvement -216.14495.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, winning-months, worst-month, stressed-net-profit,
  // stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=5989.04765, closed=725, forced=4, win=68.83%, exp=8.260755, PF=2.013, DD=0.43%, Sharpe=5.456,
  // gross=6697.69381, costs=708.64616
  // historical 2025-12..2026-06: net=619.32506, closed=208, forced=2, win=67.31%, exp=2.977524, PF=1.239, DD=1.90%, Sharpe=0.942,
  // gross=829.17077, costs=209.84571
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s13_optimized_v7 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 86, phase = 46, power = 2)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 44),
        stdDevLength = 50,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 33, smoothingType = ValueTransformation.SMA(length = 45)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 64.0,
        lowerBoundary = 31.0
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

  // GA-optimized params for s13 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-1821-s13_ga_explore.md (final rank 1 + highest all-fold fitness; shuffle=true).
  // All-fold search score 0.979801; absolute validation score 0.000000; first seen=1.
  // Fitness deltas vs target: search +0.010858; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 1.44898 vs H=120.00000; DD increase -0.68446471 pp;
  // winning months 3/4; fixed-trade stressed net -238.33088; improvement -13.10939.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, worst-month, stressed-net-profit, stressed-net-improvement,
  // absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6013.05124, closed=755, forced=3, win=67.29%, exp=7.964306, PF=1.861, DD=0.46%, Sharpe=4.053,
  // gross=6751.70132, costs=738.65008
  // historical 2025-12..2026-06: net=1459.26421, closed=217, forced=2, win=70.05%, exp=6.724720, PF=1.634, DD=1.63%, Sharpe=2.086,
  // gross=1676.06468, costs=216.80048
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s13_optimized_v8 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 78, phase = 31, power = 2)),
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
        lowerBoundary = 28.0
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

  // GA-optimized params for s13 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-0523-s13_ga_refine.md (final rank 1 + highest all-fold fitness; shuffle=false).
  // All-fold search score 0.978058; absolute validation score 0.000000; first seen=126.
  // Fitness deltas vs target: search +0.009115; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 4.34898 vs H=120.00000; DD increase -0.68451290 pp;
  // winning months 3/4; fixed-trade stressed net -235.43088; improvement -10.20939.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, worst-month, stressed-net-profit, stressed-net-improvement,
  // absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=5988.33412, closed=754, forced=3, win=67.11%, exp=7.942088, PF=1.863, DD=0.46%, Sharpe=4.026,
  // gross=6726.08096, costs=737.74685
  // historical 2025-12..2026-06: net=1385.44158, closed=215, forced=2, win=69.30%, exp=6.443914, PF=1.596, DD=1.70%, Sharpe=1.968,
  // gross=1600.52116, costs=215.07958
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s13_optimized_v9 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 78, phase = 35, power = 2)),
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
        lowerBoundary = 28.0
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

  // GA-optimized params for s13 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-0603-s13_ga_explore.md (final rank 1 + highest all-fold fitness; shuffle=true).
  // All-fold search score 0.970530; absolute validation score 0.000000; first seen=88.
  // Fitness deltas vs target: search +0.001587; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 4.34898 vs H=120.00000; DD increase -0.68451290 pp;
  // winning months 3/4; fixed-trade stressed net -235.43088; improvement -10.20939.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, worst-month, stressed-net-profit, stressed-net-improvement,
  // absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=5517.26380, closed=752, forced=3, win=66.62%, exp=7.336787, PF=1.760, DD=0.47%, Sharpe=3.648,
  // gross=6252.58568, costs=735.32188
  // historical 2025-12..2026-06: net=1459.06462, closed=216, forced=2, win=69.44%, exp=6.754929, PF=1.640, DD=1.69%, Sharpe=2.090,
  // gross=1674.86626, costs=215.80165
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s13_optimized_v10 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 79, phase = 13, power = 2)),
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
        lowerBoundary = 28.0
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
  // Measured as a lineage reference. Historical net (637) is lower than the replacement s4_optimized_v2 (990).
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

  // GA-optimized indicator params for s4_optimized_v2 (rules unchanged; shuffle=true). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-0110-s4_optimized_v2_ga_explore.md
  // training 0.567081 -> validation 0.000000, retaining 0.0%; first searched generation 64.
  // Absent from final shortlist; validation is a diagnostic replay, not a selection verdict.
  // Retained alongside its base after user review on 2026-10-06; the other 40 batch additions were removed.
  // Diagnostic reference, not a selected champion.
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
  // gross=4243.39033, costs=598.03600; base s4_optimized_v2 net=3708.07348.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1119.60856, closed=167, forced=3, win=71.86%, exp=6.704243, PF=1.666, DD=0.81%, Sharpe=2.015
  // gross=1284.47820, costs=164.86964; base s4_optimized_v2 net=990.17144.
  val s4_optimized_v7 = TestStrategy(
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

  // GA-optimized params for s4_optimized_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-0108-s4_optimized_v2_ga_refine.md (final rank 1 + highest all-fold fitness; shuffle=false).
  // All-fold search score 0.569920; absolute validation score 0.000000; first seen=82.
  // ga-optimisation-2026-10-10-1327-s4_optimized_v2_ga_refine.md (final rank 1 + highest all-fold fitness; shuffle=false).
  // All-fold search score 0.569920; absolute validation score 0.000000; first seen=106.
  // ga-optimisation-2026-10-10-1356-s4_optimized_v2_ga_explore.md (final rank 1 + highest all-fold fitness; shuffle=true).
  // All-fold search score 0.569920; absolute validation score 0.000000; first seen=78.
  // Fitness deltas vs target: search +0.006584; validation +0.000000 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 133.64457 vs H=120.00000; DD increase -0.13889236 pp;
  // winning months 3/4; fixed-trade stressed net -153.24084; improvement 133.14565.
  // Rejected: absolute-fitness, positive-net-profit, stressed-net-profit, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=3912.93678, closed=612, forced=2, win=70.59%, exp=6.393688, PF=1.638, DD=0.40%, Sharpe=2.758,
  // gross=4512.82126, costs=599.88448
  // historical 2025-12..2026-06: net=892.94243, closed=168, forced=2, win=69.64%, exp=5.315134, PF=1.508, DD=1.03%, Sharpe=1.592,
  // gross=1059.35572, costs=166.41329
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s4_optimized_v8 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 50, phase = -27, power = 1)),
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

  // GA-optimized params for s6_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-1504-s6_optimized_ga_explore.md (final rank 1; shuffle=true).
  // All-fold search score 0.894677; absolute validation score 0.202307; first seen=120.
  // Fitness deltas vs target: search +0.079951; validation +0.032054 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 175.89835 vs H=120.00000; DD increase 0.11351844 pp;
  // winning months 3/4; fixed-trade stressed net 330.47828; improvement 177.16490.
  // Rejected: drawdown-increase, worst-month, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=5814.40594, closed=815, forced=6, win=68.71%, exp=7.134240, PF=1.701, DD=0.73%, Sharpe=3.116,
  // gross=6610.24794, costs=795.84200
  // historical 2025-12..2026-06: net=872.33765, closed=217, forced=2, win=69.12%, exp=4.019989, PF=1.304, DD=2.31%, Sharpe=1.173,
  // gross=1089.69048, costs=217.35284
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s6_optimized_v9 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 85, phase = 14, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = ValueTransformation.SMA(length = 73)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 64.0,
        lowerBoundary = 32.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 10))
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

  // GA-optimized params for s6_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-0206-s6_optimized_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 1.017119; absolute validation score 0.000000; first seen=95.
  // Fitness deltas vs target: search +0.202393; validation -0.170253 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -313.39286 vs H=120.00000; DD increase 0.38037667 pp;
  // winning months 1/4; fixed-trade stressed net -158.30749; improvement -311.62087.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6127.32521, closed=816, forced=5, win=70.59%, exp=7.508977, PF=1.736, DD=0.52%, Sharpe=3.848,
  // gross=6923.16754, costs=795.84233
  // historical 2025-12..2026-06: net=611.65747, closed=214, forced=2, win=67.29%, exp=2.858212, PF=1.203, DD=2.21%, Sharpe=0.908,
  // gross=826.59341, costs=214.93595
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s6_optimized_v10 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 76, phase = -17, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 22, smoothingType = ValueTransformation.SMA(length = 46)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 64.0,
        lowerBoundary = 32.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 20))
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

  // GA-optimized params for s6_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-0245-s6_optimized_ga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 0.985449; absolute validation score 0.000000; first seen=128.
  // Fitness deltas vs target: search +0.170723; validation -0.170253 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -95.65142 vs H=120.00000; DD increase 0.32011422 pp;
  // winning months 2/4; fixed-trade stressed net 59.52255; improvement -93.79084.
  // Rejected: absolute-fitness, net-improvement, drawdown-increase, winning-months, worst-month, stressed-net-improvement,
  // absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6036.34758, closed=823, forced=5, win=69.38%, exp=7.334566, PF=1.745, DD=0.62%, Sharpe=3.569,
  // gross=6837.77369, costs=801.42611
  // historical 2025-12..2026-06: net=260.75526, closed=217, forced=2, win=67.74%, exp=1.201637, PF=1.086, DD=2.54%, Sharpe=0.384,
  // gross=478.94935, costs=218.19409
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s6_optimized_v11 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 85, phase = -43, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 36),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 23, smoothingType = ValueTransformation.SMA(length = 68)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 64.0,
        lowerBoundary = 32.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 6))
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

  // GA-optimized params for s6_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-1425-s6_optimized_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 0.971873; absolute validation score 0.000000; first seen=84.
  // Fitness deltas vs target: search +0.157147; validation -0.170253 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 124.39389 vs H=120.00000; DD increase 0.11701438 pp;
  // winning months 2/4; fixed-trade stressed net 278.83492; improvement 125.52154.
  // Rejected: absolute-fitness, drawdown-increase, winning-months, worst-month, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6382.87460, closed=809, forced=6, win=69.10%, exp=7.889833, PF=1.815, DD=0.44%, Sharpe=4.214,
  // gross=7172.71881, costs=789.84420
  // historical 2025-12..2026-06: net=485.04465, closed=219, forced=2, win=68.49%, exp=2.214816, PF=1.159, DD=2.14%, Sharpe=0.679,
  // gross=705.23879, costs=220.19414
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s6_optimized_v12 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 95, phase = -38, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = ValueTransformation.SMA(length = 61)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 64.0,
        lowerBoundary = 32.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 6))
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

  // GA-optimized params for s6_optimized (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-1504-s6_optimized_ga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 0.963331; absolute validation score 0.000000; first seen=134.
  // Fitness deltas vs target: search +0.148605; validation -0.170253 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement -455.56472 vs H=120.00000; DD increase 0.57112406 pp;
  // winning months 2/4; fixed-trade stressed net -300.60913; improvement -453.92251.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6258.27085, closed=816, forced=6, win=70.34%, exp=7.669450, PF=1.769, DD=0.51%, Sharpe=3.539,
  // gross=7055.04502, costs=796.77417
  // historical 2025-12..2026-06: net=-185.79873, closed=212, forced=2, win=63.68%, exp=-0.876409, PF=0.946, DD=2.78%, Sharpe=-0.271,
  // gross=26.86260, costs=212.66133
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s6_optimized_v13 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 82, phase = 24, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 18, smoothingType = ValueTransformation.SMA(length = 58)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 64.0,
        lowerBoundary = 32.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 10))
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

  // GA-optimized params for s5_optimized_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-2015-s5_optimized_v2_ga_explore.md (final rank 1; shuffle=true).
  // All-fold search score 0.844620; absolute validation score 0.199758; first seen=56.
  // Fitness deltas vs target: search +0.631384; validation +0.036577 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 289.31733 vs H=120.00000; DD increase 0.37256480 pp;
  // winning months 4/4; fixed-trade stressed net 396.68146; improvement 270.91320.
  // Rejected: drawdown-increase, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6229.19899, closed=759, forced=4, win=66.40%, exp=8.207113, PF=1.754, DD=0.34%, Sharpe=5.174,
  // gross=6970.44910, costs=741.25011
  // historical 2025-12..2026-06: net=1436.83507, closed=232, forced=0, win=66.38%, exp=6.193255, PF=1.539, DD=0.53%, Sharpe=4.208,
  // gross=1670.05842, costs=233.22335
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s5_optimized_v10 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 58, phase = 8, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 34),
        stdDevLength = 35,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 30, smoothingType = ValueTransformation.SMA(length = 63)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 67.0,
        lowerBoundary = 29.0
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

  // GA-optimized params for s5_optimized_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-10-1249-s5_optimized_v2_ga_explore.md (final rank 1; shuffle=true).
  // All-fold search score 0.844436; absolute validation score 0.189063; first seen=148.
  // Fitness deltas vs target: search +0.631200; validation +0.025882 (rounded report scores).
  // Reported baseline matches: none; catalogue matches: none.
  // Selection net improvement 101.51195 vs H=120.00000; DD increase 0.64596372 pp;
  // winning months 2/4; fixed-trade stressed net 211.25073; improvement 85.48247.
  // Rejected: net-improvement, drawdown-increase, winning-months, worst-month, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=6134.54861, closed=771, forced=5, win=67.45%, exp=7.956613, PF=1.750, DD=0.42%, Sharpe=3.672,
  // gross=6888.57993, costs=754.03131
  // historical 2025-12..2026-06: net=1248.11321, closed=233, forced=0, win=65.24%, exp=5.356709, PF=1.459, DD=0.82%, Sharpe=3.832,
  // gross=1482.76399, costs=234.65078
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s5_optimized_v11 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 58, phase = 8, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 33),
        stdDevLength = 34,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 28, smoothingType = ValueTransformation.SMA(length = 56)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 67.0,
        lowerBoundary = 29.0
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

  // GA-optimized params for s5_optimized_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-1936-s5_optimized_v2_ga_refine.md (final rank 1; shuffle=false).
  // All-fold search score 0.371641; absolute validation score 0.187057; first seen=0.
  // ga-optimisation-2026-10-09-2352-s5_optimized_v2_ga_refine.md (final rank 1; shuffle=false).
  // All-fold search score 0.371641; absolute validation score 0.187057; first seen=0.
  // ga-optimisation-2026-10-10-0030-s5_optimized_v2_ga_explore.md (final rank 1; shuffle=true).
  // All-fold search score 0.371641; absolute validation score 0.187057; first seen=0.
  // ga-optimisation-2026-10-10-1211-s5_optimized_v2_ga_refine.md (final rank 1; shuffle=false).
  // All-fold search score 0.371641; absolute validation score 0.187057; first seen=0.
  // Fitness deltas vs target: search +0.158405; validation +0.023876 (rounded report scores).
  // Reported baseline matches: s6; catalogue matches: none.
  // Selection net improvement 77.98159 vs H=120.00000; DD increase 0.33857499 pp;
  // winning months 3/4; fixed-trade stressed net 209.49134; improvement 83.72309.
  // Rejected: net-improvement, drawdown-increase, worst-month, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=4554.63508, closed=445, forced=3, win=66.74%, exp=10.235135, PF=1.989, DD=0.43%, Sharpe=3.451,
  // gross=4986.68850, costs=432.05341
  // historical 2025-12..2026-06: net=776.67981, closed=124, forced=1, win=58.87%, exp=6.263547, PF=1.635, DD=0.78%, Sharpe=3.736,
  // gross=902.41510, costs=125.73529
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s5_optimized_v12 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 90, phase = -6, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 41,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = ValueTransformation.SMA(length = 50)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 11),
        upperBoundary = 66.0,
        lowerBoundary = 30.0
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

  // GA-optimized params for s5_optimized_v2 (rules unchanged); diagnostic-only reference.
  // BASE RETAINED; no approved upgrade. Objective Current/search-objective-v1; policy trial-v1 (research settings).
  // ga-optimisation-2026-10-09-1936-s5_optimized_v2_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 0.919141; absolute validation score 0.000000; first seen=0.
  // ga-optimisation-2026-10-09-2015-s5_optimized_v2_ga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 0.919141; absolute validation score 0.000000; first seen=0.
  // ga-optimisation-2026-10-09-2352-s5_optimized_v2_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 0.919141; absolute validation score 0.000000; first seen=0.
  // ga-optimisation-2026-10-10-0030-s5_optimized_v2_ga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 0.919141; absolute validation score 0.000000; first seen=0.
  // ga-optimisation-2026-10-10-1211-s5_optimized_v2_ga_refine.md (highest all-fold fitness; shuffle=false).
  // All-fold search score 0.919141; absolute validation score 0.000000; first seen=0.
  // ga-optimisation-2026-10-10-1249-s5_optimized_v2_ga_explore.md (highest all-fold fitness; shuffle=true).
  // All-fold search score 0.919141; absolute validation score 0.000000; first seen=0.
  // Fitness deltas vs target: search +0.705905; validation -0.163181 (rounded report scores).
  // Reported baseline matches: s5_optimized_v3 (fixed inputs restored); catalogue matches: s5_optimized_v3 (after restoring fixed
  // inputs).
  // Selection net improvement -326.66237 vs H=120.00000; DD increase 0.58301078 pp;
  // winning months 1/4; fixed-trade stressed net -218.05441; improvement -343.82267.
  // Rejected: absolute-fitness, positive-net-profit, net-improvement, drawdown-increase, winning-months, worst-month,
  // stressed-net-profit, stressed-net-improvement, absolute-constraint.
  // Exact breaches, fold comparisons and all source evidence: docs/ga-promotions-2026-10-11.md.
  // searched 2023-07..2025-07: net=8307.27112, closed=768, forced=5, win=68.49%, exp=10.816759, PF=2.093, DD=0.31%, Sharpe=5.398,
  // gross=9059.53274, costs=752.26161
  // historical 2025-12..2026-06: net=1435.84805, closed=236, forced=0, win=66.53%, exp=6.084102, PF=1.528, DD=0.81%, Sharpe=3.132,
  // gross=1674.07677, costs=238.22872
  // Historical period reused for development and promotion; continuous results do not change the recorded rejection.
  val s5_optimized_v13 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 58, phase = 8, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 33),
        stdDevLength = 34,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 26, smoothingType = ValueTransformation.SMA(length = 55)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 68.0,
        lowerBoundary = 29.0
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
