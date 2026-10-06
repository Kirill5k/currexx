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
  * The 2026-10-06 batch adds 41 distinct candidates from the 30 reports dated 2026-10-04..06: 14 selected champions and 27 diagnostic
  * references. Each report contributes its top finalist and distinct highest recorded all-fold fitness candidate, including best-seen
  * candidates absent from the shortlist. Exact stored-indicator and rule duplicates reuse their existing entries. All additions remain in
  * the batch for comparison; no base is replaced. See docs/ga-promotions-2026-10-06.md for report mappings and measurements.
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

  // GA-optimized indicator params for s10_v2 (rules unchanged; shuffle=false). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-0623-s10_v2_ga_refine.md
  // training 0.846400 -> validation 0.000000, retaining 0.0%; first searched generation 136.
  // Absent from final shortlist; validation is a diagnostic replay, not a selection verdict.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -1089.611797927638391455151752338839, required > 0
  //   - expectancy is -8.85863250, required > 0
  //   - median period profit is -371.819455245014024420045875735025, required > 0
  //   - profitable pair-months is 0.250, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.2565458754991625897035276483781358, required >= 1.3
  //   - profit factor is 0.64171, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.167, required >= 0.667
  // Validation fold vs target: net=-804.35050; closed=+37; forced=+2; costs=+37.62370; portfolio drawdown=+1.31 percentage points; constraint breaches=10 -> 9.
  // Candidate vs target, all-fold training fitness: +0.554294 (+189.76%).
  // Candidate vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=6213.09438, closed=729, forced=5, win=69.41%, exp=8.522763, PF=1.536, DD=0.37%, Sharpe=3.606
  // gross=6928.37179, costs=715.27741; base s10_v2 net=4139.07418.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=634.12476, closed=213, forced=0, win=61.97%, exp=2.977112, PF=1.163, DD=0.87%, Sharpe=1.702
  // gross=846.11681, costs=211.99205; base s10_v2 net=1613.31781.
  val s10_v2_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 75, phase = -14, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 34),
        stdDevLength = 35,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 27, smoothingType = ValueTransformation.SMA(length = 54)),
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

  // GA-optimized indicator params for s10_v2 (rules unchanged; shuffle=false). Top #1, diagnostic only, from
  // ga-optimisation-2026-10-05-0623-s10_v2_ga_refine.md
  // training 0.829349 -> validation 0.000000, retaining 0.0%; first searched generation 53.
  // Final shortlist rank 1; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -1178.686109051954819869488580917706, required > 0
  //   - expectancy is -8.99760389, required > 0
  //   - median period profit is -296.095257086882656127145271126045, required > 0
  //   - profitable pair-months is 0.250, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.2125560628340193352627232092869369, required >= 1.3
  //   - profit factor is 0.63476, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.537244 (+183.92%).
  // Leading finalist vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=-893.42481; closed=+45; forced=+2; costs=+45.78789; portfolio drawdown=+1.28 percentage points; constraint breaches=10 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=5098.95707, closed=796, forced=6, win=66.96%, exp=6.405725, PF=1.392, DD=0.49%, Sharpe=3.015
  // gross=5880.09768, costs=781.14062; base s10_v2 net=4139.07418.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1447.57847, closed=235, forced=0, win=65.96%, exp=6.159908, PF=1.375, DD=0.80%, Sharpe=2.582
  // gross=1681.04466, costs=233.46618; base s10_v2 net=1613.31781.
  val s10_v2_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 72, phase = 3, power = 2)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 34),
        stdDevLength = 35,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 25, smoothingType = ValueTransformation.SMA(length = 44)),
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

  // GA-optimized indicator params for s10_v2 (rules unchanged; shuffle=false). Top #1, diagnostic only, from
  // ga-optimisation-2026-10-05-2241-s10_v2_ga_refine.md
  // training 0.748216 -> validation 0.000000, retaining 0.0%; first searched generation 83.
  // Final shortlist rank 1; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 10 constraint(s) on validation data:
  //   - closed trades is 102, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - net profit is -1068.112617383194154673069211807633, required > 0
  //   - expectancy is -10.47169233, required > 0
  //   - median period profit is -238.200886087986243790953117443035, required > 0
  //   - profitable pair-months is 0.250, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.1710692210161290466867498347560379, required >= 1.3
  //   - profit factor is 0.59225, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.000, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.456110 (+156.15%).
  // Leading finalist vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=-782.85132; closed=+16; forced=+1; costs=+15.05423; portfolio drawdown=+1.19 percentage points; constraint breaches=10 -> 10.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=6090.39094, closed=593, forced=6, win=67.12%, exp=10.270474, PF=1.601, DD=0.43%, Sharpe=3.104
  // gross=6666.08725, costs=575.69631; base s10_v2 net=4139.07418.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=487.75480, closed=152, forced=2, win=57.90%, exp=3.208913, PF=1.174, DD=1.22%, Sharpe=0.860
  // gross=641.19993, costs=153.44513; base s10_v2 net=1613.31781.
  val s10_v2_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 86, phase = -16, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 30),
        stdDevLength = 42,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 28, smoothingType = ValueTransformation.SMA(length = 44)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 15)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 11),
        upperBoundary = 67.0,
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

  // GA-optimized indicator params for s10_v2 (rules unchanged; shuffle=true). Top #1, diagnostic only, from
  // ga-optimisation-2026-10-05-2325-s10_v2_ga_explore.md
  // training 0.891905 -> validation 0.000000, retaining 0.0%; first searched generation 139.
  // Final shortlist rank 1; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -294.0691255711880192413063822559590, required > 0
  //   - expectancy is -2.39080590, required > 0
  //   - median period profit is -148.35624249769173604866005223890, required > 0
  //   - profitable pair-months is 0.417, required >= 0.550
  //   - most concentrated pair's best month is 0.924, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.5849723752920149579584039314493904, required >= 1.3
  //   - profit factor is 0.80818, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.599799 (+205.34%).
  // Leading finalist vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=-8.80783; closed=+37; forced=+0; costs=+37.06211; portfolio drawdown=+0.13 percentage points; constraint breaches=10 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=4561.40966, closed=648, forced=1, win=70.83%, exp=7.039212, PF=1.773, DD=0.27%, Sharpe=4.938
  // gross=5189.92716, costs=628.51750; base s10_v2 net=4139.07418.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=546.35550, closed=182, forced=1, win=68.13%, exp=3.001953, PF=1.315, DD=0.52%, Sharpe=2.649
  // gross=727.67545, costs=181.31995; base s10_v2 net=1613.31781.
  val s10_v2_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 99, phase = -13, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 31),
        stdDevLength = 36,
        stdDevMultiplier = 2.5
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 19, smoothingType = ValueTransformation.SMA(length = 43)),
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

  // SCGA-optimized indicator params for s10_v2 (rules unchanged; shuffle=true). Top #1, diagnostic only, from
  // scga-optimisation-2026-10-06-0009-s10_v2_scga_explore.md
  // training 1.008413 -> validation 0.000000, retaining 0.0%; first searched generation 124.
  // Final shortlist rank 1; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -776.2797341747184746775834916027508, required > 0
  //   - expectancy is -5.10710351, required > 0
  //   - median period profit is -185.258337411108045756351481099055, required > 0
  //   - profitable pair-months is 0.333, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.2350436980498407389221698407333352, required >= 1.3
  //   - profit factor is 0.58935, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.167, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.716308 (+245.22%).
  // Leading finalist vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=-491.01844; closed=+66; forced=+0; costs=+65.83015; portfolio drawdown=+0.33 percentage points; constraint breaches=10 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=4310.81375, closed=796, forced=2, win=74.62%, exp=5.415595, PF=1.827, DD=0.26%, Sharpe=3.668
  // gross=5084.74603, costs=773.93228; base s10_v2 net=4139.07418.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=169.51144, closed=235, forced=3, win=66.81%, exp=0.721325, PF=1.082, DD=0.95%, Sharpe=0.642
  // gross=400.24877, costs=230.73733; base s10_v2 net=1613.31781.
  val s10_v2_optimized_v6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 73, phase = 14, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 93),
        stdDevLength = 47,
        stdDevMultiplier = 3.0
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 5, smoothingType = ValueTransformation.SMA(length = 19)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 20)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 5),
        upperBoundary = 68.0,
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

  // GA-optimized indicator params for s10_optimized (rules unchanged; shuffle=false). Champion from
  // ga-optimisation-2026-10-04-1514-s10_optimized_ga_refine.md
  // training 0.496703 -> validation 0.404651, retaining 81.5%; first searched generation 150.
  // Final shortlist rank 1; training rank 11.
  // BREACHES 2 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  // Leading finalist vs target, all-fold training fitness: -0.303427 (-37.92%).
  // Leading finalist vs target, validation fitness: +0.404651 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s10): -0.260769 (-34.43%).
  // Leading finalist vs strongest seed on validation (s10): +0.404651 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+677.13575; closed=-21; forced=+0; costs=-20.66729; portfolio drawdown=-1.52 percentage points; constraint breaches=9 -> 2.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=3254.68198, closed=715, forced=5, win=67.83%, exp=4.552003, PF=1.482, DD=0.55%, Sharpe=2.200
  // gross=3952.42672, costs=697.74474; base s10_optimized net=5690.56723.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=621.64549, closed=202, forced=2, win=63.37%, exp=3.077453, PF=1.288, DD=1.02%, Sharpe=2.013
  // gross=825.88071, costs=204.23522; base s10_optimized net=1027.44697.
  val s10_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 87, phase = 58, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 32),
        stdDevLength = 42,
        stdDevMultiplier = 2.7
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 21, smoothingType = ValueTransformation.SMA(length = 49)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 15)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 5),
        upperBoundary = 69.0,
        lowerBoundary = 37.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 7))
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

  // GA-optimized indicator params for s10_optimized (rules unchanged; shuffle=true). Champion from
  // ga-optimisation-2026-10-05-1757-s10_optimized_ga_explore.md
  // training 0.767925 -> validation 0.282990, retaining 36.9%; first searched generation 100.
  // Final shortlist rank 1; training rank 21.
  // BREACHES 2 constraint(s) on validation data:
  //   - profitable pair-months is 0.542, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  // Leading finalist vs target, all-fold training fitness: -0.032205 (-4.03%).
  // Leading finalist vs target, validation fitness: +0.282990 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s10): +0.010452 (+1.38%).
  // Leading finalist vs strongest seed on validation (s10): +0.282990 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+562.85734; closed=-8; forced=+1; costs=-8.73002; portfolio drawdown=-1.30 percentage points; constraint breaches=9 -> 2.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=3467.09019, closed=846, forced=4, win=67.73%, exp=4.098215, PF=1.455, DD=0.41%, Sharpe=2.803
  // gross=4293.72727, costs=826.63708; base s10_optimized net=5690.56723.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=498.80182, closed=231, forced=2, win=63.20%, exp=2.159315, PF=1.208, DD=0.59%, Sharpe=1.575
  // gross=733.49218, costs=234.69036; base s10_optimized net=1027.44697.
  val s10_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 87, phase = 55, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 35,
        stdDevMultiplier = 2.7
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 21, smoothingType = ValueTransformation.SMA(length = 40)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 16)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 5),
        upperBoundary = 68.0,
        lowerBoundary = 44.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 11))
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

  // GA-optimized indicator params for s10_optimized (rules unchanged; shuffle=true). Top #1, diagnostic only, from
  // ga-optimisation-2026-10-04-1737-s10_optimized_ga_explore.md
  // training 0.800130 -> validation 0.000000, retaining 0.0%; first searched generation 1.
  // Final shortlist rank 1; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -299.9128839060233134619808983633595, required > 0
  //   - expectancy is -1.94748626, required > 0
  //   - median period profit is -215.71787954600857549339824025950, required > 0
  //   - profitable pair-months is 0.333, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.6789054773936020030947787373555200, required >= 1.3
  //   - profit factor is 0.89283, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.167, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.000000 (+0.00%).
  // Leading finalist vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s10): +0.042658 (+5.63%).
  // Leading finalist vs strongest seed on validation (s10): +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+0.00000; closed=+0; forced=+0; costs=+0.00000; portfolio drawdown=+0.00 percentage points; constraint breaches=9 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=5690.56723, closed=863, forced=6, win=67.90%, exp=6.593937, PF=1.479, DD=0.47%, Sharpe=2.859
  // gross=6530.49034, costs=839.92311; base s10_optimized net=5690.56723.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1027.44697, closed=234, forced=2, win=63.68%, exp=4.390799, PF=1.286, DD=1.45%, Sharpe=2.688
  // gross=1263.35802, costs=235.91106; base s10_optimized net=1027.44697.
  val s10_optimized_v4 = TestStrategy(
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
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 7))
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

  // GA-optimized indicator params for s10_optimized (rules unchanged; shuffle=false). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-1709-s10_optimized_ga_refine.md
  // training 0.804195 -> validation 0.000000, retaining 0.0%; first searched generation 88.
  // Final shortlist rank 4; training rank 4.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -41.92621582639507537035127567294210, required > 0
  //   - expectancy is -0.27049172, required > 0
  //   - median period profit is -213.629426159167353474188465869565, required > 0
  //   - profitable pair-months is 0.375, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.9529213387025736238066817077969627, required >= 1.3
  //   - profit factor is 0.98421, required >= 1.2
  //   - costs as a share of gross profit is 1.373, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Best all-fold search candidate vs target, all-fold training fitness: +0.004065 (+0.51%).
  // Best all-fold search candidate vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Best all-fold search candidate vs strongest seed on training (s10): +0.046722 (+6.17%).
  // Best all-fold search candidate vs strongest seed on validation (s10): +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+257.98667; closed=+1; forced=+0; costs=+1.25700; portfolio drawdown=-0.02 percentage points; constraint breaches=9 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=5663.45735, closed=864, forced=6, win=67.94%, exp=6.554927, PF=1.474, DD=0.53%, Sharpe=2.859
  // gross=6504.48249, costs=841.02513; base s10_optimized net=5690.56723.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1098.37216, closed=235, forced=2, win=64.26%, exp=4.673924, PF=1.306, DD=1.43%, Sharpe=3.108
  // gross=1335.55096, costs=237.17880; base s10_optimized net=1027.44697.
  val s10_optimized_v5 = TestStrategy(
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

  // GA-optimized indicator params for s10_optimized (rules unchanged; shuffle=false). Top #1, diagnostic only, from
  // ga-optimisation-2026-10-05-1709-s10_optimized_ga_refine.md
  // training 0.804195 -> validation 0.000000, retaining 0.0%; first searched generation 150.
  // Final shortlist rank 1; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -41.70312260007751940877082445281760, required > 0
  //   - expectancy is -0.27079950, required > 0
  //   - median period profit is -213.517879546008575493398240259515, required > 0
  //   - profitable pair-months is 0.375, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.9531601147090703622452590882371740, required >= 1.3
  //   - profit factor is 0.98429, required >= 1.2
  //   - costs as a share of gross profit is 1.375, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.004065 (+0.51%).
  // Leading finalist vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s10): +0.046722 (+6.17%).
  // Leading finalist vs strongest seed on validation (s10): +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+258.20976; closed=+0; forced=+0; costs=+0.00103; portfolio drawdown=-0.02 percentage points; constraint breaches=9 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=5663.45735, closed=864, forced=6, win=67.94%, exp=6.554927, PF=1.474, DD=0.53%, Sharpe=2.859
  // gross=6504.48249, costs=841.02513; base s10_optimized net=5690.56723.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1098.37216, closed=235, forced=2, win=64.26%, exp=4.673924, PF=1.306, DD=1.43%, Sharpe=3.108
  // gross=1335.55096, costs=237.17880; base s10_optimized net=1027.44697.
  val s10_optimized_v6 = TestStrategy(
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

  // SCGA-optimized indicator params for s1_v2_optimized (rules unchanged; shuffle=true). Champion from
  // scga-optimisation-2026-10-06-0427-s1_v2_optimized_scga_explore.md
  // training 1.058488 -> validation 0.070930, retaining 6.7%; first searched generation 147.
  // Final shortlist rank 1; training rank 12.
  // BREACHES 4 constraint(s) on validation data:
  //   - profitable pair-months is 0.375, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.12213, required >= 1.2
  //   - profitable datasets is 0.500, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.150054 (+16.52%).
  // Leading finalist vs target, validation fitness: +0.070930 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s1_v2_optimized_v4): +0.413721 (+64.17%).
  // Leading finalist vs strongest seed on validation (s1_v2_optimized_v4): +0.057725 (+437.16%).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+807.94571; closed=-48; forced=+0; costs=-49.71466; portfolio drawdown=-1.52 percentage points; constraint breaches=9 -> 4.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=10864.29673, closed=989, forced=9, win=52.58%, exp=10.985133, PF=1.655, DD=0.98%, Sharpe=3.926
  // gross=11833.71146, costs=969.41473; base s1_v2_optimized net=9094.96794.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=455.93232, closed=294, forced=6, win=51.02%, exp=1.550790, PF=1.071, DD=3.22%, Sharpe=0.326
  // gross=750.84728, costs=294.91496; base s1_v2_optimized net=1443.13093.
  val s1_v2_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 19, phase = -21, power = 7),
        line2Transformation = ValueTransformation.JMA(length = 5, phase = 100, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 38),
        upperBoundary = 67.0,
        lowerBoundary = 36.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 44)),
      Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = ValueTransformation.SMA(length = 25))
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged; shuffle=false). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-06-0307-s1_v2_optimized_ga_refine.md
  // training 0.958056 -> validation 0.000000, retaining 0.0%; first searched generation 148.
  // Final shortlist rank 2; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -776.5222742064810675730181098230279, required > 0
  //   - expectancy is -3.15659461, required > 0
  //   - median period profit is -200.09356888701737749318540599936, required > 0
  //   - profitable pair-months is 0.417, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.5680623015403181263545266366830137, required >= 1.3
  //   - profit factor is 0.81888, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Shortlist training leader vs target, all-fold training fitness: +0.049622 (+5.46%).
  // Shortlist training leader vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Shortlist training leader vs strongest seed on training (s1_v2_optimized_v4): +0.313289 (+48.59%).
  // Shortlist training leader vs strongest seed on validation (s1_v2_optimized_v4): -0.013205 (-100.00%).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=-373.30029; closed=+11; forced=+0; costs=+10.45157; portfolio drawdown=+0.22 percentage points; constraint breaches=9 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=9114.21879, closed=1457, forced=11, win=50.52%, exp=6.255469, PF=1.428, DD=0.85%, Sharpe=3.116
  // gross=10547.04960, costs=1432.83080; base s1_v2_optimized net=9094.96794.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1213.47915, closed=392, forced=6, win=51.79%, exp=3.095610, PF=1.179, DD=1.66%, Sharpe=2.340
  // gross=1604.87732, costs=391.39818; base s1_v2_optimized net=1443.13093.
  val s1_v2_optimized_v6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 13, phase = 36, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 62, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 81.0,
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged; shuffle=true). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-06-0347-s1_v2_optimized_ga_explore.md
  // training 0.953687 -> validation 0.000000, retaining 0.0%; first searched generation 118.
  // Final shortlist rank 4; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -387.6248356481456036551696299819725, required > 0
  //   - expectancy is -1.64946739, required > 0
  //   - median period profit is -141.24356888701737749318540599936, required > 0
  //   - profitable pair-months is 0.458, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.7727736730740775477208536023793712, required >= 1.3
  //   - profit factor is 0.90185, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Shortlist training leader vs target, all-fold training fitness: +0.045253 (+4.98%).
  // Shortlist training leader vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Shortlist training leader vs strongest seed on training (s1_v2_optimized_v4): +0.308920 (+47.91%).
  // Shortlist training leader vs strongest seed on validation (s1_v2_optimized_v4): -0.013205 (-100.00%).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+15.59714; closed=+0; forced=+0; costs=-0.00100; portfolio drawdown=-0.05 percentage points; constraint breaches=9 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=9154.15966, closed=1421, forced=11, win=50.11%, exp=6.442055, PF=1.436, DD=0.85%, Sharpe=3.107
  // gross=10552.10277, costs=1397.94311; base s1_v2_optimized net=9094.96794.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1320.47304, closed=384, forced=6, win=51.04%, exp=3.438732, PF=1.199, DD=1.71%, Sharpe=1.900
  // gross=1705.17977, costs=384.70673; base s1_v2_optimized net=1443.13093.
  val s1_v2_optimized_v7 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 13, phase = 35, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 51, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 14),
        upperBoundary = 83.0,
        lowerBoundary = 26.0
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

  // SCGA-optimized indicator params for s1_v2_optimized (rules unchanged; shuffle=true). Highest all-fold fitness reference from
  // scga-optimisation-2026-10-06-0427-s1_v2_optimized_scga_explore.md
  // training 1.110846 -> validation 0.000000, retaining 0.0%; first searched generation 110.
  // Final shortlist rank 5; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 7 constraint(s) on validation data:
  //   - median period profit is -21.98910478862613944612395840027, required > 0
  //   - profitable pair-months is 0.333, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 1.044465894790487707003514220408687, required >= 1.3
  //   - profit factor is 1.01963, required >= 1.2
  //   - costs as a share of gross profit is 0.731, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Shortlist training leader vs target, all-fold training fitness: +0.202412 (+22.28%).
  // Shortlist training leader vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Shortlist training leader vs strongest seed on training (s1_v2_optimized_v4): +0.466079 (+72.29%).
  // Shortlist training leader vs strongest seed on validation (s1_v2_optimized_v4): -0.013205 (-100.00%).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+473.83175; closed=-43; forced=+0; costs=-43.93451; portfolio drawdown=-1.04 percentage points; constraint breaches=9 -> 7.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=11049.44114, closed=1037, forced=9, win=51.98%, exp=10.655199, PF=1.655, DD=0.94%, Sharpe=3.764
  // gross=12066.47619, costs=1017.03506; base s1_v2_optimized net=9094.96794.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=-166.52701, closed=312, forced=6, win=50.00%, exp=-0.533740, PF=0.976, DD=3.07%, Sharpe=-0.118
  // gross=147.94818, costs=314.47518; base s1_v2_optimized net=1443.13093.
  val s1_v2_optimized_v8 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 19, phase = -21, power = 7),
        line2Transformation = ValueTransformation.JMA(length = 5, phase = 97, power = 9)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 38),
        upperBoundary = 67.0,
        lowerBoundary = 36.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 47)),
      Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = ValueTransformation.SMA(length = 25))
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

  // GA-optimized indicator params for s13 (rules unchanged; shuffle=false). Top #1, diagnostic only, from
  // ga-optimisation-2026-10-06-0053-s13_ga_refine.md
  // training 1.009274 -> validation 0.000000, retaining 0.0%; first searched generation 94.
  // Final shortlist rank 1; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 10 constraint(s) on validation data:
  //   - closed trades is 109, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - net profit is -178.1401061104617860846909651004370, required > 0
  //   - expectancy is -1.63431290, required > 0
  //   - median period profit is -58.67672799531670062573978631896, required > 0
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.7650649982019368036486055378622750, required >= 1.3
  //   - profit factor is 0.87479, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.040331 (+4.16%).
  // Leading finalist vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s13_optimized): +0.043764 (+4.53%).
  // Leading finalist vs strongest seed on validation (s13_optimized): +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=-2.84362; closed=+7; forced=-2; costs=+8.66486; portfolio drawdown=-0.72 percentage points; constraint breaches=10 -> 10.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=6314.62914, closed=736, forced=5, win=70.25%, exp=8.579659, PF=2.049, DD=0.45%, Sharpe=4.583
  // gross=7035.44810, costs=720.81896; base s13 net=7216.75462.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=526.55353, closed=206, forced=2, win=65.05%, exp=2.556085, PF=1.215, DD=1.32%, Sharpe=0.986
  // gross=732.23375, costs=205.68022; base s13 net=578.42388.
  val s13_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 70, phase = 33, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 46),
        stdDevLength = 50,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 30, smoothingType = ValueTransformation.SMA(length = 43)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 63.0,
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

  // GA-optimized indicator params for s13 (rules unchanged; shuffle=true). Top #1, diagnostic only, from
  // ga-optimisation-2026-10-06-0138-s13_ga_explore.md
  // training 0.972293 -> validation 0.000000, retaining 0.0%; first searched generation 93.
  // Final shortlist rank 1; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -170.9475090145602174899188395814676, required > 0
  //   - expectancy is -1.32517449, required > 0
  //   - median period profit is -54.78514996604900739423757831044, required > 0
  //   - profitable pair-months is 0.542, required >= 0.550
  //   - most concentrated pair's best month is 0.877, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.7872587371285000553129062821667602, required >= 1.3
  //   - profit factor is 0.90186, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.003350 (+0.35%).
  // Leading finalist vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s13_optimized): +0.006783 (+0.70%).
  // Leading finalist vs strongest seed on validation (s13_optimized): +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+4.34898; closed=+27; forced=-2; costs=+29.11675; portfolio drawdown=-0.68 percentage points; constraint breaches=10 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=5974.38986, closed=757, forced=3, win=67.37%, exp=7.892193, PF=1.849, DD=0.46%, Sharpe=4.025
  // gross=6715.13895, costs=740.74909; base s13 net=7216.75462.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1443.16421, closed=216, forced=2, win=69.91%, exp=6.681316, PF=1.627, DD=1.63%, Sharpe=2.081
  // gross=1658.96468, costs=215.80048; base s13 net=578.42388.
  val s13_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 78, phase = 29, power = 2)),
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

  // SCGA-optimized indicator params for s13 (rules unchanged; shuffle=true). Top #1, diagnostic only, from
  // scga-optimisation-2026-10-06-0223-s13_scga_explore.md
  // training 1.038828 -> validation 0.000000, retaining 0.0%; first searched generation 136.
  // Final shortlist rank 1; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -149.9826409439554170026536993753339, required > 0
  //   - expectancy is -0.99326252, required > 0
  //   - median period profit is -42.94251253374725466392976099853, required > 0
  //   - profitable pair-months is 0.458, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.8116122557019164583469639091106899, required >= 1.3
  //   - profit factor is 0.91544, required >= 1.2
  //   - costs as a share of gross profit is 113.885, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.069885 (+7.21%).
  // Leading finalist vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s13_optimized): +0.073318 (+7.59%).
  // Leading finalist vs strongest seed on validation (s13_optimized): +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+25.31385; closed=+49; forced=+1; costs=+51.46128; portfolio drawdown=-0.85 percentage points; constraint breaches=10 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=5406.75783, closed=877, forced=2, win=68.19%, exp=6.165060, PF=1.752, DD=0.60%, Sharpe=4.081
  // gross=6261.42018, costs=854.66235; base s13 net=7216.75462.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=384.25350, closed=237, forced=2, win=62.87%, exp=1.621323, PF=1.149, DD=1.16%, Sharpe=1.483
  // gross=623.36226, costs=239.10877; base s13 net=578.42388.
  val s13_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 69, phase = 98, power = 3)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 45),
        stdDevLength = 34,
        stdDevMultiplier = 2.8
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 16, smoothingType = ValueTransformation.SMA(length = 29)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 66.0,
        lowerBoundary = 38.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.CMF(length = 40))
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

  // GA-optimized indicator params for s4_optimized_v2 (rules unchanged; shuffle=false). Champion from
  // ga-optimisation-2026-10-04-2349-s4_optimized_v2_ga_refine.md
  // training 0.231465 -> validation 0.069334, retaining 30.0%; first searched generation 150.
  // Final shortlist rank 1; training rank 14.
  // BREACHES 4 constraint(s) on validation data:
  //   - closed trades is 105, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.10801, required >= 1.2
  //   - costs as a share of gross profit is 0.412, required <= 0.400
  // Leading finalist vs target, all-fold training fitness: -0.331872 (-58.91%).
  // Leading finalist vs target, validation fitness: +0.069334 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s4_optimized_v1): -0.174144 (-42.93%).
  // Leading finalist vs strongest seed on validation (s4_optimized_v1): +0.069334 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+375.24774; closed=-16; forced=+0; costs=-16.50499; portfolio drawdown=-0.43 percentage points; constraint breaches=8 -> 4.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=2786.36558, closed=539, forced=4, win=68.09%, exp=5.169509, PF=1.372, DD=0.50%, Sharpe=1.762
  // gross=3311.75109, costs=525.38550; base s4_optimized_v2 net=3708.07348.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=172.49353, closed=151, forced=1, win=65.56%, exp=1.142341, PF=1.077, DD=1.13%, Sharpe=0.396
  // gross=322.75793, costs=150.26441; base s4_optimized_v2 net=990.17144.
  val s4_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 57, phase = -50, power = 1)),
      Indicator
        .KeltnerChannel(source = ValueSource.Close, middleBand = ValueTransformation.EMA(length = 28), atrLength = 21, atrMultiplier = 2.5),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 12),
        upperBoundary = 73.0,
        lowerBoundary = 24.0
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

  // GA-optimized indicator params for s4_optimized_v2 (rules unchanged; shuffle=false). Champion from
  // ga-optimisation-2026-10-05-2010-s4_optimized_v2_ga_refine.md
  // training 0.509255 -> validation 0.008952, retaining 1.8%; first searched generation 61.
  // Final shortlist rank 1; training rank 19.
  // BREACHES 6 constraint(s) on validation data:
  //   - closed trades is 115, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - most concentrated pair's best month is 0.936, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 1.171192447843061614534607070945960, required >= 1.3
  //   - profit factor is 1.06465, required >= 1.2
  //   - costs as a share of gross profit is 0.590, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: -0.054081 (-9.60%).
  // Leading finalist vs target, validation fitness: +0.008952 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s4_optimized_v1): +0.103647 (+25.55%).
  // Leading finalist vs strongest seed on validation (s4_optimized_v1): +0.008952 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+305.67398; closed=-6; forced=+0; costs=-6.26226; portfolio drawdown=-0.48 percentage points; constraint breaches=8 -> 6.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=3236.49573, closed=606, forced=2, win=69.80%, exp=5.340752, PF=1.495, DD=0.42%, Sharpe=2.416
  // gross=3828.68273, costs=592.18700; base s4_optimized_v2 net=3708.07348.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=700.11232, closed=166, forced=1, win=68.68%, exp=4.217544, PF=1.400, DD=1.02%, Sharpe=1.343
  // gross=864.03525, costs=163.92293; base s4_optimized_v2 net=990.17144.
  val s4_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 50, phase = -20, power = 1)),
      Indicator
        .KeltnerChannel(source = ValueSource.Close, middleBand = ValueTransformation.EMA(length = 28), atrLength = 24, atrMultiplier = 2.5),
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

  // GA-optimized indicator params for s4_optimized_v2 (rules unchanged; shuffle=true). Champion from
  // ga-optimisation-2026-10-05-0110-s4_optimized_v2_ga_explore.md
  // training 0.188037 -> validation 0.005861, retaining 3.1%; first searched generation 150.
  // Final shortlist rank 1; training rank 21.
  // BREACHES 5 constraint(s) on validation data:
  //   - closed trades is 114, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 1.103658522958674135863186876608869, required >= 1.3
  //   - profit factor is 1.05441, required >= 1.2
  //   - costs as a share of gross profit is 0.607, required <= 0.400
  // Leading finalist vs target, all-fold training fitness: -0.375299 (-66.62%).
  // Leading finalist vs target, validation fitness: +0.005861 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s4_optimized_v1): -0.217571 (-53.64%).
  // Leading finalist vs strongest seed on validation (s4_optimized_v1): +0.005861 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+299.60354; closed=-7; forced=-1; costs=-7.28845; portfolio drawdown=-0.05 percentage points; constraint breaches=8 -> 5.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=2984.95166, closed=560, forced=3, win=70.54%, exp=5.330271, PF=1.468, DD=0.51%, Sharpe=1.761
  // gross=3532.78918, costs=547.83751; base s4_optimized_v2 net=3708.07348.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=595.19618, closed=154, forced=2, win=70.78%, exp=3.864910, PF=1.300, DD=1.39%, Sharpe=0.922
  // gross=745.99945, costs=150.80327; base s4_optimized_v2 net=990.17144.
  val s4_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 61, phase = -37, power = 1)),
      Indicator
        .KeltnerChannel(source = ValueSource.Close, middleBand = ValueTransformation.EMA(length = 28), atrLength = 20, atrMultiplier = 2.5),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 13),
        upperBoundary = 70.0,
        lowerBoundary = 33.0
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 26, smoothingType = ValueTransformation.SMA(length = 44))
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

  // GA-optimized indicator params for s4_optimized_v2 (rules unchanged; shuffle=false). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-04-2349-s4_optimized_v2_ga_refine.md
  // training 0.569920 -> validation 0.000000, retaining 0.0%; first searched generation 76.
  // Absent from final shortlist; validation is a diagnostic replay, not a selection verdict.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 8 constraint(s) on validation data:
  //   - net profit is -92.14951161542464838071003529915370, required > 0
  //   - expectancy is -0.75532387, required > 0
  //   - profitable pair-months is 0.542, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.8546348103612479782268418875601258, required >= 1.3
  //   - profit factor is 0.93701, required >= 1.2
  //   - costs as a share of gross profit is 4.068, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Validation fold vs target: net=+133.64457; closed=+1; forced=+0; costs=+0.99785; portfolio drawdown=-0.14 percentage points; constraint breaches=8 -> 8.
  // Candidate vs target, all-fold training fitness: +0.006584 (+1.17%).
  // Candidate vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Candidate vs strongest seed on training (s4_optimized_v1): +0.164312 (+40.51%).
  // Candidate vs strongest seed on validation (s4_optimized_v1): +0.000000 (n/a (baseline is zero)).
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=3912.93678, closed=612, forced=2, win=70.59%, exp=6.393688, PF=1.638, DD=0.40%, Sharpe=2.758
  // gross=4512.82126, costs=599.88448; base s4_optimized_v2 net=3708.07348.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=892.94243, closed=168, forced=2, win=69.64%, exp=5.315134, PF=1.508, DD=1.03%, Sharpe=1.592
  // gross=1059.35572, costs=166.41329; base s4_optimized_v2 net=990.17144.
  val s4_optimized_v6 = TestStrategy(
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

  // GA-optimized indicator params for s4_optimized_v2 (rules unchanged; shuffle=true). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-0110-s4_optimized_v2_ga_explore.md
  // training 0.567081 -> validation 0.000000, retaining 0.0%; first searched generation 64.
  // Absent from final shortlist; validation is a diagnostic replay, not a selection verdict.
  // Retained for comparison at user request; not a selected champion.
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

  // GA-optimized indicator params for s6_optimized (rules unchanged; shuffle=true). Champion from
  // ga-optimisation-2026-10-05-2157-s6_optimized_ga_explore.md
  // training 0.939308 -> validation 0.210929, retaining 22.5%; first searched generation 107.
  // Final shortlist rank 1; training rank 19.
  // BREACHES 1 constraint(s) on validation data:
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  // Leading finalist vs target, all-fold training fitness: +0.124581 (+15.29%).
  // Leading finalist vs target, validation fitness: +0.040676 (+23.89%).
  // Leading finalist vs strongest seed on training (s6): +0.678204 (+259.75%).
  // Leading finalist vs strongest seed on validation (s5_optimized_v2): +0.210929 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+169.58705; closed=-2; forced=+0; costs=-1.74545; portfolio drawdown=+0.02 percentage points; constraint breaches=3 -> 1.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=5977.99608, closed=820, forced=5, win=70.37%, exp=7.290239, PF=1.720, DD=0.62%, Sharpe=3.709
  // gross=6777.82429, costs=799.82822; base s6_optimized net=6206.10276.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=351.11418, closed=218, forced=1, win=67.43%, exp=1.610616, PF=1.108, DD=2.55%, Sharpe=0.479
  // gross=570.03968, costs=218.92550; base s6_optimized net=939.46582.
  val s6_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 76, phase = -17, power = 1)),
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
        lowerBoundary = 31.0
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

  // GA-optimized indicator params for s6_optimized (rules unchanged; shuffle=true). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-2157-s6_optimized_ga_explore.md
  // training 0.987992 -> validation 0.194240, retaining 19.7%; first searched generation 127.
  // Final shortlist rank 2; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 1 constraint(s) on validation data:
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  // Shortlist training leader vs target, all-fold training fitness: +0.173266 (+21.27%).
  // Shortlist training leader vs target, validation fitness: +0.023987 (+14.09%).
  // Shortlist training leader vs strongest seed on training (s6): +0.726889 (+278.39%).
  // Shortlist training leader vs strongest seed on validation (s5_optimized_v2): +0.194240 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+169.53819; closed=-1; forced=+0; costs=-1.02001; portfolio drawdown=+0.16 percentage points; constraint breaches=3 -> 1.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=6480.12634, closed=820, forced=6, win=70.85%, exp=7.902593, PF=1.823, DD=0.68%, Sharpe=3.730
  // gross=7280.04621, costs=799.91987; base s6_optimized net=6206.10276.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=419.76385, closed=218, forced=1, win=66.51%, exp=1.925522, PF=1.134, DD=2.57%, Sharpe=0.623
  // gross=637.85590, costs=218.09205; base s6_optimized net=939.46582.
  val s6_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 76, phase = -17, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
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

  // GA-optimized indicator params for s6_optimized (rules unchanged; shuffle=false). Champion from
  // ga-optimisation-2026-10-05-0231-s6_optimized_ga_refine.md
  // training 0.682992 -> validation 0.009292, retaining 1.4%; first searched generation 150.
  // Final shortlist rank 1; training rank 21.
  // BREACHES 5 constraint(s) on validation data:
  //   - profitable pair-months is 0.542, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 1.145055779607477735451799274435631, required >= 1.3
  //   - profit factor is 1.05981, required >= 1.2
  //   - costs as a share of gross profit is 0.569, required <= 0.400
  // Leading finalist vs target, all-fold training fitness: -0.131734 (-16.17%).
  // Leading finalist vs target, validation fitness: -0.160961 (-94.54%).
  // Leading finalist vs strongest seed on training (s6): +0.421889 (+161.58%).
  // Leading finalist vs strongest seed on validation (s5_optimized_v2): +0.009292 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=-108.56219; closed=+9; forced=+1; costs=+8.95992; portfolio drawdown=+0.23 percentage points; constraint breaches=3 -> 5.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=5524.17540, closed=853, forced=6, win=69.52%, exp=6.476173, PF=1.693, DD=0.62%, Sharpe=3.347
  // gross=6355.69328, costs=831.51789; base s6_optimized net=6206.10276.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=-71.09399, closed=220, forced=2, win=62.27%, exp=-0.323154, PF=0.978, DD=2.53%, Sharpe=-0.107
  // gross=149.28497, costs=220.37895; base s6_optimized net=939.46582.
  val s6_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 79, phase = -4, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 41,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 19, smoothingType = ValueTransformation.SMA(length = 54)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 64.0,
        lowerBoundary = 37.0
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

  // GA-optimized indicator params for s6_optimized (rules unchanged; shuffle=false). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-0231-s6_optimized_ga_refine.md
  // training 0.961673 -> validation 0.000000, retaining 0.0%; first searched generation 11.
  // Absent from final shortlist; validation is a diagnostic replay, not a selection verdict.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -28.23298327431848290951881066258834, required > 0
  //   - expectancy is -0.19337660, required > 0
  //   - median period profit is -87.675597619905772336914554457165, required > 0
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.9676717605567507882731889741524234, required >= 1.3
  //   - profit factor is 0.98711, required >= 1.2
  //   - costs as a share of gross profit is 1.240, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Validation fold vs target: net=-254.91882; closed=-1; forced=+0; costs=-1.00950; portfolio drawdown=+0.19 percentage points; constraint breaches=3 -> 9.
  // Candidate vs target, all-fold training fitness: +0.146946 (+18.04%).
  // Candidate vs target, validation fitness: -0.170253 (-100.00%).
  // Candidate vs strongest seed on training (s6): +0.700569 (+268.31%).
  // Candidate vs strongest seed on validation (s5_optimized_v2): +0.000000 (n/a (baseline is zero)).
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=6193.15806, closed=823, forced=5, win=68.89%, exp=7.525101, PF=1.748, DD=0.49%, Sharpe=4.293
  // gross=6995.72293, costs=802.56487; base s6_optimized net=6206.10276.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=822.81867, closed=220, forced=2, win=69.09%, exp=3.740085, PF=1.293, DD=2.04%, Sharpe=1.250
  // gross=1043.75025, costs=220.93158; base s6_optimized net=939.46582.
  val s6_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 97, phase = -41, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 36),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 22, smoothingType = ValueTransformation.SMA(length = 48)),
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

  // GA-optimized indicator params for s6_optimized (rules unchanged; shuffle=true). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-0427-s6_optimized_ga_explore.md
  // training 1.025915 -> validation 0.000000, retaining 0.0%; first searched generation 105.
  // Absent from final shortlist; validation is a diagnostic replay, not a selection verdict.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -170.8668509257937140485662531433565, required > 0
  //   - expectancy is -1.13911234, required > 0
  //   - median period profit is -133.180414294537153946334504662855, required > 0
  //   - profitable pair-months is 0.375, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.8416545557885644245706384274688808, required >= 1.3
  //   - profit factor is 0.92676, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Validation fold vs target: net=-397.55269; closed=+3; forced=+2; costs=+2.77401; portfolio drawdown=+0.45 percentage points; constraint breaches=3 -> 9.
  // Candidate vs target, all-fold training fitness: +0.211189 (+25.92%).
  // Candidate vs target, validation fitness: -0.170253 (-100.00%).
  // Candidate vs strongest seed on training (s6): +0.764811 (+292.92%).
  // Candidate vs strongest seed on validation (s5_optimized_v2): +0.000000 (n/a (baseline is zero)).
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=6704.99771, closed=830, forced=5, win=69.16%, exp=8.078311, PF=1.798, DD=0.52%, Sharpe=4.153
  // gross=7515.49128, costs=810.49357; base s6_optimized net=6206.10276.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=311.67716, closed=219, forced=2, win=66.21%, exp=1.423183, PF=1.104, DD=2.40%, Sharpe=0.460
  // gross=531.61120, costs=219.93404; base s6_optimized net=939.46582.
  val s6_optimized_v6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 100, phase = -35, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 36),
        stdDevLength = 41,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 22, smoothingType = ValueTransformation.SMA(length = 46)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 63.0,
        lowerBoundary = 32.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 7))
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

  // GA-optimized indicator params for s6_optimized (rules unchanged; shuffle=true). Top #1, diagnostic only, from
  // ga-optimisation-2026-10-05-0427-s6_optimized_ga_explore.md
  // training 1.018091 -> validation 0.000000, retaining 0.0%; first searched generation 117.
  // Final shortlist rank 1; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -132.8727092606064248598568620551263, required > 0
  //   - expectancy is -0.87995172, required > 0
  //   - median period profit is -114.183343461943509351979809118735, required > 0
  //   - profitable pair-months is 0.375, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.8723706096626549213893106743713745, required >= 1.3
  //   - profit factor is 0.94271, required >= 1.2
  //   - costs as a share of gross profit is 8.656, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.203365 (+24.96%).
  // Leading finalist vs target, validation fitness: -0.170253 (-100.00%).
  // Leading finalist vs strongest seed on training (s6): +0.756988 (+289.92%).
  // Leading finalist vs strongest seed on validation (s5_optimized_v2): +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=-359.55855; closed=+4; forced=+2; costs=+3.48412; portfolio drawdown=+0.39 percentage points; constraint breaches=3 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=6913.11767, closed=833, forced=5, win=69.27%, exp=8.299061, PF=1.840, DD=0.62%, Sharpe=3.965
  // gross=7727.01942, costs=813.90175; base s6_optimized net=6206.10276.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=442.51381, closed=222, forced=2, win=67.12%, exp=1.993305, PF=1.149, DD=2.40%, Sharpe=0.606
  // gross=665.16438, costs=222.65057; base s6_optimized net=939.46582.
  val s6_optimized_v7 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 100, phase = -35, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 36),
        stdDevLength = 41,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 23, smoothingType = ValueTransformation.SMA(length = 46)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 63.0,
        lowerBoundary = 32.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 9))
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

  // GA-optimized indicator params for s6_optimized (rules unchanged; shuffle=false). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-2114-s6_optimized_ga_refine.md
  // training 0.961471 -> validation 0.000000, retaining 0.0%; first searched generation 32.
  // Final shortlist rank 6; training rank 5.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -172.0591333313754592414957736268685, required > 0
  //   - expectancy is -1.19485509, required > 0
  //   - median period profit is -105.430250311589221427774512573345, required > 0
  //   - profitable pair-months is 0.458, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.8170640878975566328213272790822418, required >= 1.3
  //   - profit factor is 0.92398, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Best all-fold search candidate vs target, all-fold training fitness: +0.146745 (+18.01%).
  // Best all-fold search candidate vs target, validation fitness: -0.170253 (-100.00%).
  // Best all-fold search candidate vs strongest seed on training (s6): +0.700368 (+268.23%).
  // Best all-fold search candidate vs strongest seed on validation (s5_optimized_v2): +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=-398.74497; closed=-3; forced=+0; costs=-3.54025; portfolio drawdown=+0.54 percentage points; constraint breaches=3 -> 9.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=6293.43291, closed=813, forced=6, win=69.99%, exp=7.741000, PF=1.779, DD=0.49%, Sharpe=3.668
  // gross=7087.07675, costs=793.64384; base s6_optimized net=6206.10276.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=321.02830, closed=213, forced=2, win=66.20%, exp=1.507175, PF=1.103, DD=2.59%, Sharpe=0.472
  // gross=534.68043, costs=213.65213; base s6_optimized net=939.46582.
  val s6_optimized_v8 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 77, phase = -18, power = 1)),
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

  // GA-optimized indicator params for s2_optimized (rules unchanged; shuffle=false). Champion from
  // ga-optimisation-2026-10-04-1248-s2_optimized_ga_refine.md
  // training 0.727891 -> validation 0.404436, retaining 55.6%; first searched generation 150.
  // Final shortlist rank 1; training rank 23.
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 0.880, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.18938, required >= 1.2
  // Leading finalist vs target, all-fold training fitness: -0.218000 (-23.05%).
  // Leading finalist vs target, validation fitness: +0.404436 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s2_optimized_v2): +0.566186 (+350.13%).
  // Leading finalist vs strongest seed on validation (s2_optimized_v2): +0.404436 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+1038.64356; closed=+18; forced=+0; costs=+15.67274; portfolio drawdown=-0.83 percentage points; constraint breaches=8 -> 3.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=9541.19449, closed=1581, forced=12, win=38.58%, exp=6.034911, PF=1.412, DD=1.10%, Sharpe=2.388
  // gross=11097.62192, costs=1556.42743; base s2_optimized net=12225.94093.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=2188.50942, closed=469, forced=6, win=38.59%, exp=4.666331, PF=1.305, DD=1.36%, Sharpe=2.311
  // gross=2652.05779, costs=463.54837; base s2_optimized net=2713.33834.
  val s2_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 38, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 25, phase = -18, power = 1)
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

  // GA-optimized indicator params for s2_optimized (rules unchanged; shuffle=true). Champion from
  // ga-optimisation-2026-10-04-1403-s2_optimized_ga_explore.md
  // training 0.773623 -> validation 0.297538, retaining 38.5%; first searched generation 150.
  // Final shortlist rank 1; training rank 12.
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 0.947, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.17336, required >= 1.2
  // Leading finalist vs target, all-fold training fitness: -0.172268 (-18.21%).
  // Leading finalist vs target, validation fitness: +0.297538 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s2_optimized_v2): +0.611917 (+378.41%).
  // Leading finalist vs strongest seed on validation (s2_optimized_v2): +0.297538 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+965.17557; closed=+14; forced=+0; costs=+12.68191; portfolio drawdown=-0.81 percentage points; constraint breaches=8 -> 3.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=10336.77002, closed=1582, forced=12, win=39.57%, exp=6.533989, PF=1.451, DD=1.11%, Sharpe=2.569
  // gross=11894.25866, costs=1557.48864; base s2_optimized net=12225.94093.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1967.32139, closed=468, forced=6, win=38.25%, exp=4.203678, PF=1.267, DD=1.38%, Sharpe=2.145
  // gross=2430.40372, costs=463.08233; base s2_optimized net=2713.33834.
  val s2_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 15, phase = 80, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 24, phase = -18, power = 1)
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

  // GA-optimized indicator params for s2_optimized (rules unchanged; shuffle=false). Champion from
  // ga-optimisation-2026-10-05-1553-s2_optimized_ga_refine.md
  // training 1.020013 -> validation 0.009245, retaining 0.9%; first searched generation 79.
  // Final shortlist rank 1; training rank 15.
  // BREACHES 6 constraint(s) on validation data:
  //   - profitable pair-months is 0.458, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 1.210579964658578829390179831734116, required >= 1.3
  //   - profit factor is 1.05095, required >= 1.2
  //   - costs as a share of gross profit is 0.595, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Leading finalist vs target, all-fold training fitness: +0.074122 (+7.84%).
  // Leading finalist vs target, validation fitness: +0.009245 (n/a (baseline is zero)).
  // Leading finalist vs strongest seed on training (s2_optimized_v2): +0.858307 (+530.78%).
  // Leading finalist vs strongest seed on validation (s2_optimized_v2): +0.009245 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+433.90466; closed=+32; forced=+0; costs=+31.99785; portfolio drawdown=-0.57 percentage points; constraint breaches=8 -> 6.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=10585.99817, closed=1727, forced=12, win=39.95%, exp=6.129704, PF=1.455, DD=0.88%, Sharpe=3.228
  // gross=12286.06813, costs=1700.06996; base s2_optimized net=12225.94093.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=2456.77374, closed=507, forced=6, win=38.86%, exp=4.845708, PF=1.338, DD=1.31%, Sharpe=3.334
  // gross=2955.97641, costs=499.20267; base s2_optimized net=2713.33834.
  val s2_optimized_v5 = TestStrategy(
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

  // GA-optimized indicator params for s2_optimized (rules unchanged; shuffle=false). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-04-1248-s2_optimized_ga_refine.md
  // training 1.055238 -> validation 0.004574, retaining 0.4%; first searched generation 70.
  // Absent from final shortlist; validation is a diagnostic replay, not a selection verdict.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 5 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 1.145860676010855182655446596010236, required >= 1.3
  //   - profit factor is 1.03692, required >= 1.2
  //   - costs as a share of gross profit is 0.669, required <= 0.400
  // Validation fold vs target: net=+365.10973; closed=+32; forced=+0; costs=+31.99552; portfolio drawdown=-0.48 percentage points; constraint breaches=8 -> 5.
  // Candidate vs target, all-fold training fitness: +0.109347 (+11.56%).
  // Candidate vs target, validation fitness: +0.004574 (n/a (baseline is zero)).
  // Candidate vs strongest seed on training (s2_optimized_v2): +0.893533 (+552.57%).
  // Candidate vs strongest seed on validation (s2_optimized_v2): +0.004574 (n/a (baseline is zero)).
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=10766.93669, closed=1728, forced=12, win=39.93%, exp=6.230866, PF=1.462, DD=0.88%, Sharpe=3.229
  // gross=12469.41902, costs=1702.48232; base s2_optimized net=12225.94093.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=2538.09428, closed=507, forced=6, win=38.86%, exp=5.006103, PF=1.355, DD=1.31%, Sharpe=3.201
  // gross=3035.58928, costs=497.49500; base s2_optimized net=2713.33834.
  val s2_optimized_v6 = TestStrategy(
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

  // GA-optimized indicator params for s2_optimized (rules unchanged; shuffle=true). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-04-1403-s2_optimized_ga_explore.md
  // training 1.055206 -> validation 0.004255, retaining 0.4%; first searched generation 14.
  // Final shortlist rank 12; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 5 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 1.142507887767753007957805064455277, required >= 1.3
  //   - profit factor is 1.03607, required >= 1.2
  //   - costs as a share of gross profit is 0.674, required <= 0.400
  // Final training leader vs target, all-fold training fitness: +0.109315 (+11.56%).
  // Final training leader vs target, validation fitness: +0.004255 (n/a (baseline is zero)).
  // Final training leader vs strongest seed on training (s2_optimized_v2): +0.893500 (+552.55%).
  // Final training leader vs strongest seed on validation (s2_optimized_v2): +0.004255 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+360.90973; closed=+32; forced=+0; costs=+31.99552; portfolio drawdown=-0.47 percentage points; constraint breaches=8 -> 5.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=10789.73669, closed=1730, forced=12, win=39.94%, exp=6.236842, PF=1.464, DD=0.88%, Sharpe=3.210
  // gross=12494.21902, costs=1704.48232; base s2_optimized net=12225.94093.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=2538.09428, closed=507, forced=6, win=38.86%, exp=5.006103, PF=1.355, DD=1.31%, Sharpe=3.201
  // gross=3035.58928, costs=497.49500; base s2_optimized net=2713.33834.
  val s2_optimized_v7 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 14, phase = 62, power = 2),
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

  // GA-optimized indicator params for s2_optimized (rules unchanged; shuffle=false). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-1553-s2_optimized_ga_refine.md
  // training 1.024412 -> validation 0.000000, retaining 0.0%; first searched generation 19.
  // Final shortlist rank 3; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 8 constraint(s) on validation data:
  //   - net profit is -165.5700170388572198491953514008884, required > 0
  //   - expectancy is -0.49276791, required > 0
  //   - profitable pair-months is 0.458, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.8875037227097324737372116993208528, required >= 1.3
  //   - profit factor is 0.96549, required >= 1.2
  //   - costs as a share of gross profit is 1.963, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Shortlist training leader vs target, all-fold training fitness: +0.078521 (+8.30%).
  // Shortlist training leader vs target, validation fitness: +0.000000 (n/a (baseline is zero)).
  // Shortlist training leader vs strongest seed on training (s2_optimized_v2): +0.862707 (+533.50%).
  // Shortlist training leader vs strongest seed on validation (s2_optimized_v2): +0.000000 (n/a (baseline is zero)).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+16.82169; closed=+0; forced=+0; costs=+0.00230; portfolio drawdown=-0.11 percentage points; constraint breaches=8 -> 8.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=12072.59152, closed=1718, forced=12, win=40.11%, exp=7.027120, PF=1.539, DD=0.88%, Sharpe=2.837
  // gross=13776.69820, costs=1704.10668; base s2_optimized net=12225.94093.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=2599.01861, closed=468, forced=6, win=39.10%, exp=5.553459, PF=1.374, DD=1.31%, Sharpe=3.315
  // gross=3049.24293, costs=450.22431; base s2_optimized net=2713.33834.
  val s2_optimized_v8 = TestStrategy(
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged; shuffle=true). Champion from
  // ga-optimisation-2026-10-04-2155-s5_optimized_v2_ga_explore.md
  // training 0.572061 -> validation 0.331472, retaining 57.9%; first searched generation 150.
  // Final shortlist rank 1; training rank 10.
  // BREACHES 1 constraint(s) on validation data:
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  // Leading finalist vs target, all-fold training fitness: +0.358826 (+168.28%).
  // Leading finalist vs target, validation fitness: +0.168291 (+103.13%).
  // Leading finalist vs strongest seed on training (s5_optimized_v3 (fixed inputs restored)): -0.347080 (-37.76%).
  // Leading finalist vs strongest seed on validation (s6): +0.144415 (+77.20%).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+205.50780; closed=+35; forced=+0; costs=+34.27918; portfolio drawdown=+0.44 percentage points; constraint breaches=4 -> 1.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=4361.66078, closed=711, forced=2, win=63.85%, exp=6.134544, PF=1.576, DD=0.40%, Sharpe=3.409
  // gross=5057.98833, costs=696.32755; base s5_optimized_v2 net=4214.48925.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1043.66858, closed=210, forced=1, win=64.76%, exp=4.969850, PF=1.418, DD=1.16%, Sharpe=2.083
  // gross=1255.61285, costs=211.94427; base s5_optimized_v2 net=1607.43953.
  val s5_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 58, phase = 8, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 34),
        stdDevLength = 35,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 25, smoothingType = ValueTransformation.SMA(length = 56)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 10),
        upperBoundary = 64.0,
        lowerBoundary = 28.0
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged; shuffle=false). Champion from
  // ga-optimisation-2026-10-05-1846-s5_optimized_v2_ga_refine.md
  // training 0.371641 -> validation 0.187057, retaining 50.3%; first searched generation 0.
  // Final shortlist rank 1; training rank 23.
  // BREACHES 2 constraint(s) on validation data:
  //   - closed trades is 86, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  // Leading finalist vs target, all-fold training fitness: +0.158405 (+74.29%).
  // Leading finalist vs target, validation fitness: +0.023876 (+14.63%).
  // Leading finalist vs strongest seed on training (s5_optimized_v3 (fixed inputs restored)): -0.547500 (-59.57%).
  // Leading finalist vs strongest seed on validation (s6): +0.000000 (+0.00%).
  // Report baseline matches: s6; catalogue strategies: none.
  // Report parameter-only catalogue matches (different rules): s6 (exact).
  // Validation fold vs target: net=+77.98159; closed=-11; forced=-2; costs=-11.48300; portfolio drawdown=+0.34 percentage points; constraint breaches=4 -> 2.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=4554.63508, closed=445, forced=3, win=66.74%, exp=10.235135, PF=1.989, DD=0.43%, Sharpe=3.451
  // gross=4986.68850, costs=432.05341; base s5_optimized_v2 net=4214.48925.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=776.67981, closed=124, forced=1, win=58.87%, exp=6.263547, PF=1.635, DD=0.78%, Sharpe=3.736
  // gross=902.41510, costs=125.73529; base s5_optimized_v2 net=1607.43953.
  val s5_optimized_v5 = TestStrategy(
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged; shuffle=false). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-05-1846-s5_optimized_v2_ga_refine.md
  // training 0.963530 -> validation 0.170521, retaining 17.7%; first searched generation 144.
  // Final shortlist rank 2; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 4 constraint(s) on validation data:
  //   - profitable pair-months is 0.542, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.15216, required >= 1.2
  //   - profitable datasets is 0.500, required >= 0.667
  // Shortlist training leader vs target, all-fold training fitness: +0.750294 (+351.86%).
  // Shortlist training leader vs target, validation fitness: +0.007340 (+4.50%).
  // Shortlist training leader vs strongest seed on training (s5_optimized_v3 (fixed inputs restored)): +0.044389 (+4.83%).
  // Shortlist training leader vs strongest seed on validation (s6): -0.016536 (-8.84%).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+88.78225; closed=+39; forced=-2; costs=+38.73831; portfolio drawdown=+0.64 percentage points; constraint breaches=4 -> 4.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=6468.90823, closed=787, forced=5, win=67.73%, exp=8.219706, PF=1.748, DD=0.49%, Sharpe=4.072
  // gross=7237.70060, costs=768.79237; base s5_optimized_v2 net=4214.48925.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=910.65330, closed=238, forced=0, win=65.13%, exp=3.826274, PF=1.299, DD=1.31%, Sharpe=1.739
  // gross=1150.36407, costs=239.71077; base s5_optimized_v2 net=1607.43953.
  val s5_optimized_v6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 58, phase = 8, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 34),
        stdDevLength = 34,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 29, smoothingType = ValueTransformation.SMA(length = 55)),
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged; shuffle=false). Champion from
  // ga-optimisation-2026-10-04-2001-s5_optimized_v2_ga_refine.md
  // training 0.562153 -> validation 0.120370, retaining 21.4%; first searched generation 149.
  // Final shortlist rank 1; training rank 18.
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.542, required >= 0.550
  //   - most concentrated pair's best month is 0.948, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.12673, required >= 1.2
  // Leading finalist vs target, all-fold training fitness: +0.348917 (+163.63%).
  // Leading finalist vs target, validation fitness: -0.042811 (-26.24%).
  // Leading finalist vs strongest seed on training (s5_optimized_v3 (fixed inputs restored)): -0.356988 (-38.84%).
  // Leading finalist vs strongest seed on validation (s6): -0.066687 (-35.65%).
  // Report baseline matches: none; catalogue strategies: none.
  // Validation fold vs target: net=+59.69666; closed=+48; forced=-1; costs=+48.09896; portfolio drawdown=+0.87 percentage points; constraint breaches=4 -> 3.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=3792.27860, closed=791, forced=3, win=61.32%, exp=4.794284, PF=1.399, DD=0.39%, Sharpe=3.114
  // gross=4565.80359, costs=773.52499; base s5_optimized_v2 net=4214.48925.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1452.83407, closed=237, forced=0, win=66.25%, exp=6.130102, PF=1.563, DD=0.88%, Sharpe=3.958
  // gross=1690.19682, costs=237.36275; base s5_optimized_v2 net=1607.43953.
  val s5_optimized_v7 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 51, phase = -45, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 37),
        stdDevLength = 37,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 27, smoothingType = ValueTransformation.SMA(length = 59)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 9),
        upperBoundary = 66.0,
        lowerBoundary = 27.0
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged; shuffle=false). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-04-2001-s5_optimized_v2_ga_refine.md
  // training 0.928325 -> validation 0.000000, retaining 0.0%; first searched generation 138.
  // Absent from final shortlist; validation is a diagnostic replay, not a selection verdict.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -150.4773973436517190663219786269841, required > 0
  //   - expectancy is -1.09041592, required > 0
  //   - median period profit is -43.39081988455388326156249397655, required > 0
  //   - profitable pair-months is 0.458, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.7991353996644951416720038914370164, required >= 1.3
  //   - profit factor is 0.92379, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Validation fold vs target: net=-324.29351; closed=+41; forced=+0; costs=+41.78587; portfolio drawdown=+0.67 percentage points; constraint breaches=4 -> 9.
  // Candidate vs target, all-fold training fitness: +0.715089 (+335.35%).
  // Candidate vs target, validation fitness: -0.163181 (-100.00%).
  // Candidate vs strongest seed on training (s5_optimized_v3 (fixed inputs restored)): +0.009184 (+1.00%).
  // Candidate vs strongest seed on validation (s6): -0.187057 (-100.00%).
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=7064.74810, closed=789, forced=6, win=67.43%, exp=8.954053, PF=1.825, DD=0.48%, Sharpe=4.168
  // gross=7836.00323, costs=771.25512; base s5_optimized_v2 net=4214.48925.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1394.16621, closed=238, forced=0, win=67.23%, exp=5.857841, PF=1.500, DD=0.82%, Sharpe=3.843
  // gross=1634.14706, costs=239.98084; base s5_optimized_v2 net=1607.43953.
  val s5_optimized_v8 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 58, phase = 8, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 34),
        stdDevLength = 34,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 27, smoothingType = ValueTransformation.SMA(length = 62)),
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged; shuffle=true). Highest all-fold fitness reference from
  // ga-optimisation-2026-10-04-2155-s5_optimized_v2_ga_explore.md
  // training 0.919141 -> validation 0.000000, retaining 0.0%; first searched generation 0.
  // Final shortlist rank 6; training rank 1.
  // Retained for comparison at user request; not a selected champion.
  // BREACHES 8 constraint(s) on validation data:
  //   - net profit is -152.8462563596191040002379407068901, required > 0
  //   - expectancy is -1.16676532, required > 0
  //   - profitable pair-months is 0.417, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.8025374106020353189992310909246934, required >= 1.3
  //   - profit factor is 0.91987, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Final training leader vs target, all-fold training fitness: +0.705905 (+331.04%).
  // Final training leader vs target, validation fitness: -0.163181 (-100.00%).
  // Final training leader vs strongest seed on training (s5_optimized_v3 (fixed inputs restored)): +0.000000 (+0.00%).
  // Final training leader vs strongest seed on validation (s6): -0.187057 (-100.00%).
  // Report baseline matches: s5_optimized_v3 (fixed inputs restored); catalogue strategies: s5_optimized_v3 (after restoring fixed inputs).
  // Validation fold vs target: net=-326.66237; closed=+34; forced=+0; costs=+34.32060; portfolio drawdown=+0.58 percentage points; constraint breaches=4 -> 8.
  // Continuous measurements and full report comparisons: docs/ga-promotions-2026-10-06.md.
  // searched 2023-07..2025-07: net=8307.27112, closed=768, forced=5, win=68.49%, exp=10.816759, PF=2.093, DD=0.31%, Sharpe=5.398
  // gross=9059.53274, costs=752.26161; base s5_optimized_v2 net=4214.48925.
  // historical 2025-12..2026-06 (reused for s10_v2 development): net=1435.84805, closed=236, forced=0, win=66.53%, exp=6.084102, PF=1.528, DD=0.81%, Sharpe=3.132
  // gross=1674.07677, costs=238.22872; base s5_optimized_v2 net=1607.43953.
  val s5_optimized_v9 = TestStrategy(
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
