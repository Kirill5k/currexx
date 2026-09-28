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
  * The 2026-09-28 batch adds 29 distinct candidates from the 25 reports dated 2026-09-22..25: final shortlist #1 plus the final
  * training-fitness leader when different, skipping exact duplicates. All additions stay in `BatchBacktester` for comparison, including
  * reports that selected nothing. Their provenance and measurements are in docs/ga-promotions-2026-09-28.md.
  *
  * Not every val here is still measured. `BatchBacktester` holds the ones worth the runtime and is the list, rather than a copy of it kept
  * in this comment; a val it has dropped carries a `Not in BatchBacktester` line saying which val dominates it. The rest stay so that a
  * report filename still resolves to the thing it selected.
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

  // GA-optimized indicator params for s10_v2 (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-24-0025-s10_v2_ga_refine.md
  // training 0.859761 -> validation 0.000000, retaining 0.0%, unshuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -650.5291264015677531016518394767406, required > 0
  //   - expectancy is -4.71397918, required > 0
  //   - median period profit is -181.454121946823778974550548584685, required > 0
  //   - profitable pair-months is 0.375, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.5073649163338090718525252022019697, required >= 1.3
  //   - profit factor is 0.78161, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.167, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s10_v2: 695.31870 vs 1613.31781.
  // Searched net moves the other way: 5982.98190 vs 4146.27418 for the base.
  // searched 2023-07..2025-07: net=5982.98190, closed=733, forced=6, win=67.12%, exp=8.162322, PF=1.507, DD=0.36%, Sharpe=3.411
  // historical 2025-12..2026-06: net=695.31870, closed=217, forced=0, win=63.13%, exp=3.204234, PF=1.183, DD=1.13%, Sharpe=1.177
  val s10_v2_optimized = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 100, phase = -5, power = 2)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 36),
        stdDevLength = 34,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 25, smoothingType = ValueTransformation.SMA(length = 56)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 7)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 12),
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

  // GA-optimized indicator params for s10_v2 (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-24-0227-s10_v2_ga_explore.md
  // training 0.933013 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -434.3377248750208797663914511644260, required > 0
  //   - expectancy is -3.56014529, required > 0
  //   - median period profit is -188.09991220488111496125997546711, required > 0
  //   - profitable pair-months is 0.333, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.4731231943773156250535383376863247, required >= 1.3
  //   - profit factor is 0.73788, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s10_v2: 637.78252 vs 1613.31781.
  // Searched net moves the other way: 4656.66153 vs 4146.27418 for the base.
  // searched 2023-07..2025-07: net=4656.66153, closed=658, forced=2, win=71.28%, exp=7.076993, PF=1.794, DD=0.30%, Sharpe=5.334
  // historical 2025-12..2026-06: net=637.78252, closed=181, forced=0, win=65.75%, exp=3.523660, PF=1.355, DD=0.49%, Sharpe=2.023
  val s10_v2_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 97, phase = -31, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 32),
        stdDevLength = 36,
        stdDevMultiplier = 2.5
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 24, smoothingType = ValueTransformation.SMA(length = 39)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 17)),
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

  // GA-optimized indicator params for s10_v2 (rules unchanged). Top #1 and training fitness leader from
  // scga-optimisation-2026-09-24-0428-s10_v2_scga_explore.md
  // training 0.850287 -> validation 0.000000, retaining 0.0%, shuffled SCGA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -1230.699849630275629130820986026515, required > 0
  //   - expectancy is -10.08770369, required > 0
  //   - median period profit is -378.827729565348603665919082228375, required > 0
  //   - profitable pair-months is 0.250, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.1577755428473998837263659394440862, required >= 1.3
  //   - profit factor is 0.59421, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.000, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s10_v2: -499.04042 vs 1613.31781.
  // Searched net moves the other way: 5788.35612 vs 4146.27418 for the base.
  // searched 2023-07..2025-07: net=5788.35612, closed=660, forced=3, win=70.91%, exp=8.770237, PF=1.560, DD=0.37%, Sharpe=3.706
  // historical 2025-12..2026-06: net=-499.04042, closed=203, forced=1, win=62.07%, exp=-2.458327, PF=0.866, DD=1.88%, Sharpe=-0.933
  val s10_v2_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 73, phase = -69, power = 2)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 96),
        stdDevLength = 49,
        stdDevMultiplier = 2.9
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 13, smoothingType = ValueTransformation.SMA(length = 36)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 50)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 8),
        upperBoundary = 73.0,
        lowerBoundary = 25.0
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

  // GA-optimized indicator params for s10_v2 (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-24-1718-s10_v2_ga_refine.md
  // training 0.848998 -> validation 0.000000, retaining 0.0%, unshuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 10 constraint(s) on validation data:
  //   - closed trades is 110, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - net profit is -1265.120855131178942741154645374691, required > 0
  //   - expectancy is -11.50109868, required > 0
  //   - median period profit is -352.45660064479367467131179527842, required > 0
  //   - profitable pair-months is 0.167, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.1407552578459677107044350724743623, required >= 1.3
  //   - profit factor is 0.54694, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.000, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s10_v2: 761.57066 vs 1613.31781.
  // Searched net moves the other way: 6281.64685 vs 4146.27418 for the base.
  // searched 2023-07..2025-07: net=6281.64685, closed=664, forced=6, win=68.22%, exp=9.460312, PF=1.575, DD=0.43%, Sharpe=3.112
  // historical 2025-12..2026-06: net=761.57066, closed=184, forced=0, win=61.41%, exp=4.138971, PF=1.242, DD=0.79%, Sharpe=2.994
  val s10_v2_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 88, phase = -9, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 32),
        stdDevLength = 38,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 23, smoothingType = ValueTransformation.SMA(length = 48)),
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

  // GA-optimized indicator params for s10_v2 (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-24-1908-s10_v2_ga_explore.md
  // training 0.808103 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 10 constraint(s) on validation data:
  //   - closed trades is 98, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - net profit is -712.5512967691577098168730243506455, required > 0
  //   - expectancy is -7.27093160, required > 0
  //   - median period profit is -281.729183810654886855301190649975, required > 0
  //   - profitable pair-months is 0.250, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.3471027186128444294749081158458796, required >= 1.3
  //   - profit factor is 0.69924, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.167, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s10_v2: 377.28622 vs 1613.31781.
  // Searched net moves the other way: 5823.58649 vs 4146.27418 for the base.
  // searched 2023-07..2025-07: net=5823.58649, closed=619, forced=4, win=68.17%, exp=9.408056, PF=1.608, DD=0.51%, Sharpe=3.191
  // historical 2025-12..2026-06: net=377.28622, closed=167, forced=1, win=62.28%, exp=2.259199, PF=1.117, DD=1.36%, Sharpe=0.962
  val s10_v2_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 88, phase = 66, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 34),
        stdDevLength = 34,
        stdDevMultiplier = 2.5
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 23, smoothingType = ValueTransformation.SMA(length = 38)),
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

  // GA-optimized indicator params for s10_v2 (rules unchanged). Top #1 and training fitness leader from
  // scga-optimisation-2026-09-24-2055-s10_v2_scga_explore.md
  // training 0.875588 -> validation 0.000000, retaining 0.0%, shuffled SCGA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 10 constraint(s) on validation data:
  //   - closed trades is 108, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - net profit is -291.9761249314185458823613604030766, required > 0
  //   - expectancy is -2.70348264, required > 0
  //   - median period profit is -78.612957705939010977455727060875, required > 0
  //   - profitable pair-months is 0.417, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.5698018925696132732012476220046777, required >= 1.3
  //   - profit factor is 0.77866, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s10_v2: 279.75130 vs 1613.31781.
  // Searched net moves the other way: 4151.35864 vs 4146.27418 for the base.
  // searched 2023-07..2025-07: net=4151.35864, closed=623, forced=2, win=68.54%, exp=6.663497, PF=1.756, DD=0.52%, Sharpe=3.935
  // historical 2025-12..2026-06: net=279.75130, closed=179, forced=1, win=63.13%, exp=1.562856, PF=1.150, DD=0.63%, Sharpe=1.101
  val s10_v2_optimized_v6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 96, phase = 10, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 33),
        stdDevLength = 31,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 27, smoothingType = ValueTransformation.SMA(length = 37)),
      Indicator
        .ValueTracking(role = ValueRole.Volatility, source = ValueSource.Close, transformation = ValueTransformation.ATR(length = 21)),
      Indicator.ValueTracking(role = ValueRole.Price, source = ValueSource.Close, transformation = ValueTransformation.SMA(length = 1)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 5),
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

  // GA-optimized indicator params for s10_optimized (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-22-2117-s10_optimized_ga_refine.md
  // training 0.784496 -> validation 0.000000, retaining 0.0%, unshuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -312.4289844040705005243071876783691, required > 0
  //   - expectancy is -2.04201951, required > 0
  //   - median period profit is -215.71787954600857549339824025947, required > 0
  //   - profitable pair-months is 0.333, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.6655054151422801986467747220435594, required >= 1.3
  //   - profit factor is 0.88836, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.167, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net matches s10_optimized: 1073.54697.
  // searched 2023-07..2025-07: net=5646.46723, closed=862, forced=6, win=67.75%, exp=6.550426, PF=1.473, DD=0.47%, Sharpe=2.897
  // historical 2025-12..2026-06: net=1073.54697, closed=233, forced=2, win=63.52%, exp=4.607498, PF=1.299, DD=1.45%, Sharpe=2.764
  val s10_optimized_v2 = TestStrategy(
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
  // scga-optimisation-2026-09-25-1917-s1_v2_optimized_scga_explore.md
  // training 0.775874 -> validation 0.384232, retaining 49.5%, shuffled SCGA.
  // Final shortlist rank 1; training rank 18.
  // BREACHES 1 constraint(s) on validation data:
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s1_v2_optimized: -396.85146 vs 1443.13093.
  // searched 2023-07..2025-07: net=5324.18315, closed=861, forced=2, win=73.06%, exp=6.183720, PF=1.671, DD=0.43%, Sharpe=3.696
  // historical 2025-12..2026-06: net=-396.85146, closed=236, forced=1, win=70.34%, exp=-1.681574, PF=0.890, DD=1.75%, Sharpe=-0.485
  val s1_v2_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 11, phase = 90, power = 1),
        line2Transformation = ValueTransformation.JMA(length = 32, phase = -13, power = 2)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16),
        upperBoundary = 65.0,
        lowerBoundary = 45.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 44)),
      Indicator.VolatilityRegimeDetection(atrLength = 41, smoothingType = ValueTransformation.SMA(length = 29))
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
  // ga-optimisation-2026-09-25-1811-s1_v2_optimized_ga_explore.md
  // training 0.658309 -> validation 0.146384, retaining 22.2%, shuffled GA.
  // Final shortlist rank 1; training rank 25.
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.13798, required >= 1.2
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s1_v2_optimized: 890.39960 vs 1443.13093.
  // searched 2023-07..2025-07: net=8245.37006, closed=1250, forced=11, win=47.84%, exp=6.596296, PF=1.392, DD=0.86%, Sharpe=2.445
  // historical 2025-12..2026-06: net=890.39960, closed=350, forced=6, win=46.86%, exp=2.543999, PF=1.131, DD=1.94%, Sharpe=1.083
  val s1_v2_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 18, phase = 36, power = 8),
        line2Transformation = ValueTransformation.JMA(length = 9, phase = 61, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 16),
        upperBoundary = 84.0,
        lowerBoundary = 26.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 47)),
      Indicator.VolatilityRegimeDetection(atrLength = 21, smoothingType = ValueTransformation.SMA(length = 28))
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
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged). Training fitness leader from
  // ga-optimisation-2026-09-25-1701-s1_v2_optimized_ga_refine.md
  // training 0.921680 -> validation 0.000000, retaining 0.0%, unshuffled GA.
  // Final shortlist rank 2; training rank 1.
  // The report records a constraint verdict only for its top #1; this candidate has no recorded constraint verdict.
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is higher than s1_v2_optimized: 1479.19169 vs 1443.13093.
  // searched 2023-07..2025-07: net=9226.69136, closed=1434, forced=11, win=50.21%, exp=6.434234, PF=1.443, DD=0.85%, Sharpe=2.847
  // historical 2025-12..2026-06: net=1479.19169, closed=384, forced=6, win=51.30%, exp=3.852062, PF=1.220, DD=1.76%, Sharpe=2.574
  val s1_v2_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 13, phase = 36, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 61, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 15),
        upperBoundary = 88.0,
        lowerBoundary = 27.0
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged). Training fitness leader from
  // ga-optimisation-2026-09-25-1811-s1_v2_optimized_ga_explore.md
  // training 0.913693 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 2; training rank 1.
  // The report records a constraint verdict only for its top #1; this candidate has no recorded constraint verdict.
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s1_v2_optimized: 1410.46932 vs 1443.13093.
  // Searched net moves the other way: 9329.91313 vs 9094.96794 for the base.
  // searched 2023-07..2025-07: net=9329.91313, closed=1358, forced=11, win=49.12%, exp=6.870334, PF=1.446, DD=0.77%, Sharpe=2.741
  // historical 2025-12..2026-06: net=1410.46932, closed=372, forced=6, win=50.00%, exp=3.791584, PF=1.215, DD=1.99%, Sharpe=1.980
  val s1_v2_optimized_v6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 13, phase = 36, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 7, phase = 61, power = 8)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 18),
        upperBoundary = 85.0,
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

  // GA-optimized indicator params for s1_v2_optimized (rules unchanged). Training fitness leader from
  // scga-optimisation-2026-09-25-1917-s1_v2_optimized_scga_explore.md
  // training 1.106553 -> validation 0.000000, retaining 0.0%, shuffled SCGA.
  // Final shortlist rank 7; training rank 1.
  // The report records a constraint verdict only for its top #1; this candidate has no recorded constraint verdict.
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s1_v2_optimized: 616.93812 vs 1443.13093.
  // searched 2023-07..2025-07: net=6114.77676, closed=864, forced=5, win=67.94%, exp=7.077288, PF=1.813, DD=0.33%, Sharpe=4.639
  // historical 2025-12..2026-06: net=616.93812, closed=264, forced=3, win=67.42%, exp=2.336887, PF=1.224, DD=1.40%, Sharpe=1.337
  val s1_v2_optimized_v7 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 43, phase = -6, power = 6),
        line2Transformation = ValueTransformation.JMA(length = 10, phase = -36, power = 7)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 17),
        upperBoundary = 62.0,
        lowerBoundary = 40.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.RSX(length = 37)),
      Indicator.VolatilityRegimeDetection(atrLength = 33, smoothingType = ValueTransformation.SMA(length = 69))
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

  // GA-optimized indicator params for s4_optimized_v2 (rules unchanged). Top #1 from
  // ga-optimisation-2026-09-23-1925-s4_optimized_v2_ga_explore.md
  // training 0.204655 -> validation 0.292465, retaining 142.9%, shuffled GA.
  // Final shortlist rank 1; training rank 17.
  // BREACHES 2 constraint(s) on validation data:
  //   - closed trades is 119, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s4_optimized_v2: 179.26427 vs 990.17144.
  // searched 2023-07..2025-07: net=1548.77436, closed=596, forced=5, win=71.98%, exp=2.598615, PF=1.182, DD=1.11%, Sharpe=0.844
  // historical 2025-12..2026-06: net=179.26427, closed=184, forced=2, win=72.28%, exp=0.974262, PF=1.067, DD=0.89%, Sharpe=0.614
  val s4_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 79, phase = -49, power = 1)),
      Indicator
        .KeltnerChannel(source = ValueSource.Close, middleBand = ValueTransformation.EMA(length = 28), atrLength = 21, atrMultiplier = 2.3),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 13),
        upperBoundary = 70.0,
        lowerBoundary = 29.0
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 35, smoothingType = ValueTransformation.SMA(length = 61))
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged). Top #1 from
  // ga-optimisation-2026-09-23-0146-s5_optimized_v2_ga_refine.md
  // training 0.465721 -> validation 0.335742, retaining 72.1%, unshuffled GA.
  // Final shortlist rank 1; training rank 23.
  // BREACHES 1 constraint(s) on validation data:
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s5_optimized_v2: 325.65739 vs 1607.43953.
  // Searched net moves the other way: 5262.44948 vs 4221.68925 for the base.
  // searched 2023-07..2025-07: net=5262.44948, closed=653, forced=5, win=62.79%, exp=8.058881, PF=1.589, DD=0.53%, Sharpe=3.309
  // historical 2025-12..2026-06: net=325.65739, closed=182, forced=1, win=57.14%, exp=1.789326, PF=1.104, DD=1.30%, Sharpe=0.524
  val s5_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 58, phase = 73, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 33),
        stdDevLength = 34,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 26, smoothingType = ValueTransformation.SMA(length = 55)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 9),
        upperBoundary = 68.0,
        lowerBoundary = 21.0
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged). Top #1 from
  // ga-optimisation-2026-09-23-0332-s5_optimized_v2_ga_explore.md
  // training 0.479069 -> validation 0.280092, retaining 58.5%, shuffled GA.
  // Final shortlist rank 1; training rank 24.
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.458, required >= 0.550
  //   - most concentrated pair's best month is 0.895, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.18501, required >= 1.2
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s5_optimized_v2: 232.59699 vs 1607.43953.
  // Searched net moves the other way: 5144.17121 vs 4221.68925 for the base.
  // searched 2023-07..2025-07: net=5144.17121, closed=714, forced=4, win=66.25%, exp=7.204722, PF=1.650, DD=0.47%, Sharpe=3.220
  // historical 2025-12..2026-06: net=232.59699, closed=201, forced=1, win=62.19%, exp=1.157199, PF=1.078, DD=1.10%, Sharpe=0.697
  val s5_optimized_v5 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 66, phase = 1, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 34),
        stdDevLength = 35,
        stdDevMultiplier = 2.4
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 25, smoothingType = ValueTransformation.SMA(length = 55)),
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

  // GA-optimized indicator params for s5_optimized_v2 (rules unchanged). Training fitness leader from
  // ga-optimisation-2026-09-23-0146-s5_optimized_v2_ga_refine.md
  // training 0.919579 -> validation 0.000000, retaining 0.0%, unshuffled GA.
  // Final shortlist rank 6; training rank 1.
  // The report records a constraint verdict only for its top #1; this candidate has no recorded constraint verdict.
  // Also training fitness leader from
  // ga-optimisation-2026-09-23-0332-s5_optimized_v2_ga_explore.md
  // training 0.919579 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 4; training rank 1.
  // The report records a constraint verdict only for its top #1; this candidate has no recorded constraint verdict.
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s5_optimized_v2: 1435.84805 vs 1607.43953.
  // Searched net moves the other way: 8314.47112 vs 4221.68925 for the base.
  // searched 2023-07..2025-07: net=8314.47112, closed=768, forced=5, win=68.49%, exp=10.826134, PF=2.094, DD=0.31%, Sharpe=5.400
  // historical 2025-12..2026-06: net=1435.84805, closed=236, forced=0, win=66.53%, exp=6.084102, PF=1.528, DD=0.81%, Sharpe=3.132
  val s5_optimized_v6 = TestStrategy(
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

  // GA-optimized indicator params for s13 (rules unchanged). Top #1 and training fitness leader from
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
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is higher than s13: 1625.26462 vs 685.30228.
  // Searched net moves the other way: 5588.05807 vs 5951.97849 for the base.
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

  // GA-optimized indicator params for s13 (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-25-0057-s13_ga_explore.md
  // training 0.970823 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 10 constraint(s) on validation data:
  //   - closed trades is 108, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - net profit is -328.0493656861087021070558760608091, required > 0
  //   - expectancy is -3.03749413, required > 0
  //   - median period profit is -94.42087813004276035069367485014, required > 0
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.5709751826103099837390543357036490, required >= 1.3
  //   - profit factor is 0.76561, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s13: -67.98172 vs 685.30228.
  // searched 2023-07..2025-07: net=5025.47000, closed=661, forced=0, win=68.84%, exp=7.602829, PF=1.786, DD=0.85%, Sharpe=3.668
  // historical 2025-12..2026-06: net=-67.98172, closed=169, forced=2, win=62.72%, exp=-0.402259, PF=0.974, DD=1.57%, Sharpe=-0.117
  val s13_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 95, phase = -35, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 37),
        stdDevLength = 34,
        stdDevMultiplier = 2.8
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 21, smoothingType = ValueTransformation.SMA(length = 60)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 8),
        upperBoundary = 58.0,
        lowerBoundary = 36.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.CMF(length = 32))
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

  // GA-optimized indicator params for s13 (rules unchanged). Top #1 and training fitness leader from
  // scga-optimisation-2026-09-25-0320-s13_scga_explore.md
  // training 0.981961 -> validation 0.000000, retaining 0.0%, shuffled SCGA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 8 constraint(s) on validation data:
  //   - net profit is -50.55767847858808191121790116073465, required > 0
  //   - expectancy is -0.40446143, required > 0
  //   - profitable pair-months is 0.417, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.9076322387171195633460316163826389, required >= 1.3
  //   - profit factor is 0.96385, required >= 1.2
  //   - costs as a share of gross profit is 1.702, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s13: 293.48223 vs 685.30228.
  // searched 2023-07..2025-07: net=5125.81591, closed=751, forced=3, win=66.31%, exp=6.825321, PF=1.805, DD=0.59%, Sharpe=5.464
  // historical 2025-12..2026-06: net=293.48223, closed=217, forced=2, win=68.20%, exp=1.352453, PF=1.104, DD=1.49%, Sharpe=0.558
  val s13_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 49, phase = -69, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 42),
        stdDevLength = 34,
        stdDevMultiplier = 2.8
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 21, smoothingType = ValueTransformation.SMA(length = 53)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 5),
        upperBoundary = 68.0,
        lowerBoundary = 27.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.CMF(length = 28))
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

  // GA-optimized indicator params for s13 (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-25-0537-s13_ga_refine.md
  // training 0.955360 -> validation 0.000000, retaining 0.0%, unshuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 10 constraint(s) on validation data:
  //   - closed trades is 118, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - net profit is -395.8955694555438823994174189816744, required > 0
  //   - expectancy is -3.35504720, required > 0
  //   - median period profit is -102.720447043482215636151615212305, required > 0
  //   - profitable pair-months is 0.375, required >= 0.550
  //   - most concentrated pair's best month is 0.937, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.5462612618526372343909426867521372, required >= 1.3
  //   - profit factor is 0.75547, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.500, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is higher than s13: 978.49242 vs 685.30228.
  // Searched net moves the other way: 5535.92492 vs 5951.97849 for the base.
  // searched 2023-07..2025-07: net=5535.92492, closed=753, forced=3, win=68.39%, exp=7.351826, PF=1.817, DD=0.53%, Sharpe=5.623
  // historical 2025-12..2026-06: net=978.49242, closed=214, forced=2, win=68.69%, exp=4.572395, PF=1.435, DD=1.11%, Sharpe=2.325
  val s13_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 82, phase = 3, power = 2)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 46),
        stdDevLength = 48,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 17, smoothingType = ValueTransformation.SMA(length = 49)),
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

  // GA-optimized indicator params for s13 (rules unchanged). Top #1 and training fitness leader from
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
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s13: 655.02388 vs 685.30228.
  // Searched net moves the other way: 7077.95462 vs 5951.97849 for the base.
  // searched 2023-07..2025-07: net=7077.95462, closed=602, forced=2, win=72.76%, exp=11.757400, PF=2.170, DD=0.49%, Sharpe=3.985
  // historical 2025-12..2026-06: net=655.02388, closed=152, forced=2, win=66.45%, exp=4.309368, PF=1.275, DD=1.38%, Sharpe=1.268
  val s13_optimized_v5 = TestStrategy(
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

  // GA-optimized indicator params for s13 (rules unchanged). Top #1 and training fitness leader from
  // scga-optimisation-2026-09-25-1019-s13_scga_explore.md
  // training 0.966202 -> validation 0.000000, retaining 0.0%, shuffled SCGA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 10 constraint(s) on validation data:
  //   - closed trades is 117, required >= 120 (5 per pair-month over 4 months x 6 pairs)
  //   - net profit is -541.3893682879068746509039186994596, required > 0
  //   - expectancy is -4.62725956, required > 0
  //   - median period profit is -155.72956320980617391013773110297, required > 0
  //   - profitable pair-months is 0.333, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.3776028909935414274147098127370960, required >= 1.3
  //   - profit factor is 0.67965, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.167, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s13: -14.30609 vs 685.30228.
  // searched 2023-07..2025-07: net=5817.01021, closed=729, forced=4, win=67.90%, exp=7.979438, PF=1.934, DD=0.62%, Sharpe=4.627
  // historical 2025-12..2026-06: net=-14.30609, closed=208, forced=2, win=66.83%, exp=-0.068779, PF=0.995, DD=1.81%, Sharpe=-0.017
  val s13_optimized_v6 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 73, phase = -25, power = 2)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 41),
        stdDevLength = 34,
        stdDevMultiplier = 2.8
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 19, smoothingType = ValueTransformation.SMA(length = 43)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
        upperBoundary = 64.0,
        lowerBoundary = 29.0
      ),
      Indicator.ValueTracking(role = ValueRole.Momentum, source = ValueSource.Close, transformation = ValueTransformation.CMF(length = 28))
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

  // GA-optimized indicator params for s6_optimized (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-23-2041-s6_optimized_ga_refine.md
  // training 0.944782 -> validation 0.000000, retaining 0.0%, unshuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -263.1386503522418232563425293065399, required > 0
  //   - expectancy is -1.81474931, required > 0
  //   - median period profit is -90.66875664717026307797949098255, required > 0
  //   - profitable pair-months is 0.458, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.7078042984891711155379079857227776, required >= 1.3
  //   - profit factor is 0.87733, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s6_optimized: 358.79912 vs 1000.26582.
  // Searched net moves the other way: 6150.67339 vs 5787.60378 for the base.
  // searched 2023-07..2025-07: net=6150.67339, closed=835, forced=6, win=70.42%, exp=7.366076, PF=1.762, DD=0.62%, Sharpe=3.975
  // historical 2025-12..2026-06: net=358.79912, closed=217, forced=2, win=66.36%, exp=1.653452, PF=1.116, DD=2.17%, Sharpe=0.558
  val s6_optimized_v2 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 82, phase = 17, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 35),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 23, smoothingType = ValueTransformation.SMA(length = 45)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 6),
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

  // GA-optimized indicator params for s6_optimized (rules unchanged). Top #1 and training fitness leader from
  // ga-optimisation-2026-09-23-2234-s6_optimized_ga_explore.md
  // training 0.883636 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 1; training rank 1.
  // NOTHING SELECTED: no finalist scored above zero on data it was never searched against.
  // Retained for reference despite failing the validation fitness gate.
  // BREACHES 9 constraint(s) on validation data:
  //   - net profit is -347.0853835540161467108458615269227, required > 0
  //   - expectancy is -2.42717051, required > 0
  //   - median period profit is -45.301321962478667536188737971065, required > 0
  //   - profitable pair-months is 0.458, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - pair-month profit factor is 0.6373214885249416999461573436485914, required >= 1.3
  //   - profit factor is 0.83217, required >= 1.2
  //   - costs as a share of gross profit is 1.000, required <= 0.400
  //   - profitable datasets is 0.333, required >= 0.667
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s6_optimized: 587.62785 vs 1000.26582.
  // searched 2023-07..2025-07: net=5381.35222, closed=826, forced=5, win=68.52%, exp=6.514954, PF=1.648, DD=0.68%, Sharpe=3.020
  // historical 2025-12..2026-06: net=587.62785, closed=225, forced=2, win=65.78%, exp=2.611679, PF=1.211, DD=1.95%, Sharpe=0.940
  val s6_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator
        .TrendChangeDetection(source = ValueSource.HLC3, transformation = ValueTransformation.JMA(length = 81, phase = 10, power = 1)),
      Indicator.BollingerBands(
        source = ValueSource.Close,
        middleBand = ValueTransformation.SMA(length = 36),
        stdDevLength = 42,
        stdDevMultiplier = 2.6
      ),
      Indicator.VolatilityRegimeDetection(atrLength = 19, smoothingType = ValueTransformation.SMA(length = 54)),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 7),
        upperBoundary = 59.0,
        lowerBoundary = 33.0
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

  // GA-optimized indicator params for s2_optimized (rules unchanged). Top #1 from
  // ga-optimisation-2026-09-22-1924-s2_optimized_ga_refine.md
  // training 0.810938 -> validation 0.152603, retaining 18.8%, unshuffled GA.
  // Final shortlist rank 1; training rank 16.
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.458, required >= 0.550
  //   - most concentrated pair's best month is 1.000, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.14066, required >= 1.2
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s2_optimized: 1853.98628 vs 2713.33834.
  // searched 2023-07..2025-07: net=10137.15657, closed=1627, forced=12, win=39.15%, exp=6.230582, PF=1.433, DD=1.14%, Sharpe=2.669
  // historical 2025-12..2026-06: net=1853.98628, closed=486, forced=6, win=38.07%, exp=3.814787, PF=1.242, DD=1.38%, Sharpe=2.008
  val s2_optimized_v3 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 15, phase = 64, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 24, phase = -19, power = 1)
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

  // GA-optimized indicator params for s2_optimized (rules unchanged). Top #1 from
  // ga-optimisation-2026-09-22-2022-s2_optimized_ga_explore.md
  // training 0.903402 -> validation 0.134885, retaining 14.9%, shuffled GA.
  // Final shortlist rank 1; training rank 11.
  // BREACHES 3 constraint(s) on validation data:
  //   - profitable pair-months is 0.500, required >= 0.550
  //   - most concentrated pair's best month is 0.985, required <= 0.755 (0.700 scaled to 4 periods)
  //   - profit factor is 1.11646, required >= 1.2
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s2_optimized: 1635.48102 vs 2713.33834.
  // searched 2023-07..2025-07: net=11400.09478, closed=1606, forced=12, win=40.97%, exp=7.098440, PF=1.512, DD=0.85%, Sharpe=3.120
  // historical 2025-12..2026-06: net=1635.48102, closed=457, forced=6, win=40.70%, exp=3.578733, PF=1.224, DD=1.50%, Sharpe=1.973
  val s2_optimized_v4 = TestStrategy(
    indicator = Indicator.compositeAnyOf(
      Indicator.LinesCrossing(
        source = ValueSource.HLC3,
        line1Transformation = ValueTransformation.JMA(length = 17, phase = 87, power = 2),
        line2Transformation = ValueTransformation.JMA(length = 21, phase = -7, power = 1)
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.RSX(length = 29),
        upperBoundary = 72.0,
        lowerBoundary = 25.0
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

  // GA-optimized indicator params for s2_optimized (rules unchanged). Training fitness leader from
  // ga-optimisation-2026-09-22-1924-s2_optimized_ga_refine.md
  // training 1.055238 -> validation 0.004574, retaining 0.4%, unshuffled GA.
  // Final shortlist rank 9; training rank 1.
  // The report records a constraint verdict only for its top #1; this candidate has no recorded constraint verdict.
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s2_optimized: 2538.09428 vs 2713.33834.
  // searched 2023-07..2025-07: net=10766.93669, closed=1728, forced=12, win=39.93%, exp=6.230866, PF=1.462, DD=0.88%, Sharpe=3.229
  // historical 2025-12..2026-06: net=2538.09428, closed=507, forced=6, win=38.86%, exp=5.006103, PF=1.355, DD=1.31%, Sharpe=3.201
  val s2_optimized_v5 = TestStrategy(
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

  // GA-optimized indicator params for s2_optimized (rules unchanged). Training fitness leader from
  // ga-optimisation-2026-09-22-2022-s2_optimized_ga_explore.md
  // training 1.024412 -> validation 0.000000, retaining 0.0%, shuffled GA.
  // Final shortlist rank 11; training rank 1.
  // The report records a constraint verdict only for its top #1; this candidate has no recorded constraint verdict.
  // Retained in BatchBacktester for comparison; see docs/ga-promotions-2026-09-28.md.
  // Historical net is lower than s2_optimized: 2599.01861 vs 2713.33834.
  // searched 2023-07..2025-07: net=12072.59152, closed=1718, forced=12, win=40.11%, exp=7.027120, PF=1.539, DD=0.88%, Sharpe=2.837
  // historical 2025-12..2026-06: net=2599.01861, closed=468, forced=6, win=39.10%, exp=5.553459, PF=1.374, DD=1.31%, Sharpe=3.315
  val s2_optimized_v6 = TestStrategy(
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

}
