---
name: trading-strategies
description: Design a new strategy in TestStrategy.scala, or improve an existing one by hand, measuring every change with BatchBacktester and deciding on the holdout. Use when asked to "add a strategy", "come up with a new strategy", "improve s5", "why does this strategy lose money", "tune these parameters", "sweep the band multiplier", or anything else that ends in a new or changed TestStrategy val.
---

# Trading Strategies

Two jobs that share one measurement step: inventing rules that did not exist, and finding better
parameters for rules that do. They fail in different ways — a new strategy usually fails because its
indicators are not wired to the conditions its rules read, a tuned one usually fails because the
change was fitted to the years it was measured on — so settle first which of the two this is.

| Situation | Job | Section |
|---|---|---|
| No val does this thing yet | New strategy | Part 2 |
| A val does this, its parameters look arbitrary | Manual sweep | Part 3 |
| A val does this, it loses money on the holdout | Rules, not parameters | Part 2, step 6 |

Both jobs end in Part 4, which is the only part that says whether anything worked.

**Everything is measured by running `BatchBacktester` and reading its output.** There is no automated
search in this workflow: a change is made by hand, registered, measured, and kept or reverted on the
numbers. Vals named `_optimized` and comments citing `optimisation-results/*.md` are the residue of
an earlier genetic-algorithm process; they are history, not a workflow to resume.

## Paths

| Role | Path |
|---|---|
| Strategy catalogue | `modules/backtest/src/main/scala/currexx/backtest/TestStrategy.scala` |
| Batch runner | `modules/backtest/src/main/scala/currexx/backtest/BatchBacktester.scala` |
| Single-strategy report | `modules/backtest/src/main/scala/currexx/backtest/StrategyAnalyser.scala` |
| Corpora | `modules/backtest/src/main/scala/currexx/backtest/MarketDataProvider.scala` |
| Indicator / transformation types | `modules/domain/src/main/scala/currexx/domain/signal/Indicator.scala` |
| Detector primitives | `modules/domain/src/main/scala/currexx/domain/signal/Condition.scala` |
| Indicator → condition | `modules/core/src/main/scala/currexx/core/signal/SignalDetector.scala` |
| Condition → profile slot | `modules/core/src/main/scala/currexx/core/market/MarketProfileUpdater.scala` |
| Profile slots | `modules/core/src/main/scala/currexx/core/market/MarketState.scala` |
| Rules and their evaluation | `modules/core/src/main/scala/currexx/core/trade/TradeStrategy.scala` |
| Rule → order, flip logic | `modules/core/src/main/scala/currexx/core/trade/TradeService.scala` |
| Cost model and metrics | `modules/backtest/src/main/scala/currexx/backtest/OrderStatsCollector.scala` |
| Sane parameter ranges | `modules/backtest/src/main/scala/currexx/backtest/optimizer/GeneBounds.scala`, `IndicatorBounds.scala`, `ThresholdBounds.scala` |

## Part 1 — How a strategy actually fires

Read this before writing any rules. Most new strategies that produce zero trades are correct as
trading ideas and wrong here.

One bar makes one pass:

```
PriceRange -> SignalDetector.detect(indicator) -> Option[Signal(condition)]
           -> MarketProfileUpdater.update      -> one MarketProfile slot
           -> Rule.findTriggeredAction          -> Option[TradeAction]
           -> TradeService.executeAction        -> order, filled on the NEXT bar
```

`TestServices.processMarketData` runs signal detection on the previous window and executes against
the current one, so there is no lookahead and the first window of every dataset only primes the
simulator.

### The indicator you add decides which conditions can ever be true

A rule condition reads one slot of `MarketProfile`. If nothing writes that slot, the condition is
permanently false and the rule is dead code that still compiles.

| Profile slot | Written by | Conditions that read it |
|---|---|---|
| `trend` | `TrendChangeDetection` | `TrendIs`, `TrendChangedTo`, `TrendActiveFor` |
| `crossover` | `LinesCrossing` | `CrossoverOccurred` |
| `momentum` (the zone) | `ThresholdCrossing` | `MomentumIsIn`, `MomentumEntered` |
| `lastMomentumValue` | `ThresholdCrossing` on crossing bars, `ValueTracking(Momentum)` on every bar | `MomentumIs`, `ValueIs(Momentum, …)` |
| `volatility` (the regime) | `VolatilityRegimeDetection` | `VolatilityIs`, `PreviousVolatilityIs` |
| `lastBandCrossing` | `KeltnerChannel`, `BollingerBands` | `UpperBandCrossed`, `LowerBandCrossed` |
| `lastPriceLineCrossing` | `PriceLineCrossing` | `PriceCrossedLine` |
| `lastVelocityValue` | `ValueTracking(Velocity)` | `VelocityIs`, `VelocityIsBelow`, `VelocityCrossedLevel` |
| `lastVolatilityValue` | `ValueTracking(Volatility)` | `ValueIs(Volatility, …)`, `PriceMovedAgainstEntry` |
| `lastTrendStrengthValue` | `ValueTracking(TrendStrength)` | `ValueIs(TrendStrength, …)` |
| `lastChannelMiddleBandValue` | `ValueTracking(ChannelMiddleBand)` | `ValueIs(ChannelMiddleBand, …)` |
| `lastClosePrice` | `ValueTracking(Price)` | `ValueIs(Price, …)`, `PriceMovedAgainstEntry` |

`NoPosition`, `PositionIs` and `PositionOpenFor` are the exceptions: they read
`MarketState.currentPosition` and need no indicator at all.

`VolatilityRegimeDetection` writes the regime and **not** `lastVolatilityValue`, so a
`PriceMovedAgainstEntry` stop needs its own `ValueTracking(Volatility, _, ATR(n))` alongside a
`ValueTracking(Price, …)`. Nothing in the catalogue currently uses that stop.

### One slot per family, and the last writer in the composite owns it

`detectComposite` maps the children in list order and `MarketProfileUpdater` folds their conditions
in that order, so two indicators writing one slot on the same bar do not combine — the later one wins.

- Two `ThresholdCrossing`s share the momentum zone. This already went wrong: s12 carried an ADX
  threshold that silently overwrote the CMF signal its rules read, and it was deleted rather than
  fixed. Put a second oscillator on `ValueTracking(TrendStrength)` and read it with `ValueIs`, which
  is what that slot exists for.
- Two band indicators share `lastBandCrossing`, and `Condition.bandCrossing` tries the upper band
  first, so an upper cross hides a simultaneous lower one.
- Two `PriceLineCrossing`s share `lastPriceLineCrossing` even with different roles: the rule filters
  on role after the fact, so the loser of the collision is simply absent.
- Ordering is a tool, not only a hazard. s6 places `ValueTracking(Momentum)` last precisely so it
  owns `lastMomentumValue` on every bar, leaving the `ThresholdCrossing` above it to own the zone and
  write the value only when it crosses. Without that `ValueTracking`, `lastMomentumValue` changes
  only on crossing bars, so `MomentumIs` compares two crossings and reads as stale.

### `compositeAnyOf`, never `compositeAllOf`

`CombinationLogic.All` requires every child to emit on the same bar. Most detectors emit only on an
event — a regime change, a crossing, a turn — so an `All` composite of four indicators fires
approximately never. Every val in the catalogue uses `Indicator.compositeAnyOf`. The composite is a
bag of independent detectors, not a conjunction; conjunction belongs in the rules, via
`Rule.Condition.allOf`.

### `LinesCrossing` direction is reported from `line1`

`Indicator.scala` comments `line1Transformation` as SLOW and `line2Transformation` as FAST.
`Condition.crossingDirection` knows nothing about slow or fast — it reports which way `line1` moved
relative to `line2` — and the catalogue puts the **shorter** length in `line1`, so
`CrossoverOccurred(Upward)` means the fast line crossed up through the slow one. Follow the
catalogue, not the comment. Reverse the two and every rule in the strategy inverts.
`s2_optimized_v2` is the one inverted val (line1 JMA 38 against line2 JMA 23) and is out of
`BatchBacktester` on a weak holdout.

### Events and states are different conditions

One-bar edges, true only on the bar the thing happened: `TrendChangedTo`, `CrossoverOccurred`,
`MomentumEntered`, `UpperBandCrossed`, `LowerBandCrossed`, `PriceCrossedLine`,
`VelocityCrossedLevel`.

Everything else stays true for as long as the state holds: `TrendIs`, `TrendActiveFor`,
`MomentumIsIn`, `MomentumIs` (which is "momentum is rising", not "momentum just turned"),
`VolatilityIs`, `PreviousVolatilityIs`, `VelocityIs`, `VelocityIsBelow`, `ValueIs`, `PositionIs`,
`PositionOpenFor`, `NoPosition`, `PriceMovedAgainstEntry`.

An entry built entirely from edges needs them on the same bar, which is a coincidence and not a
condition. That coincidence is what held s5's reversion leg to 342 trades over 24 months; s6
loosened one of the two into a state (`MomentumIs` instead of `MomentumEntered`) and gained +1613
in-sample net.

### `NoPosition` in an open rule is what decides whether the strategy flips

`TradeService.processMarketStateUpdate` resolves close rules first, then:

- position open, close rule fired → `ClosePosition`
- position open, no close rule, **opposite** open rule fired → `FlipToLong` / `FlipToShort`, which
  exits and reverses in one bar
- flat, open rule fired → `OpenLong` / `OpenShort`

So an open rule guarded by `Rule.Condition.NoPosition` can never reverse a position — the guard makes
the rule false while one is open, and the flip case never arises. The s1_v2 and s2 families omit it
and flip; s4, s5, s6 and s12 include it and only ever enter flat. This is a structural choice about
the strategy, not boilerplate: it changes trade count, average hold time, and how much of the edge is
spent on costs.

## Part 2 — Producing a new strategy

### Step 1 — State the thesis in one sentence, and name the exit

An entry idea is not a strategy, and in this catalogue the exit is usually where the money is.
Removing s6's momentum exit costs 4255 of its 6287 in-sample net. Removing the trend exit it
inherited from s5 *gained* 824, because that exit was cutting winners short. Decide up front what
closes a position, and expect to measure that decision on its own in step 6.

### Step 2 — Choose the indicators the rules will need

Work backwards from the conditions in the thesis, through the Part 1 table, to the indicator that
writes each slot. Check the result against the collision rules before writing any Scala. A composite
of four to five indicators is the catalogue's normal size.

### Step 3 — Write the val

Copy the shape of the nearest existing val. Named arguments throughout, `Indicator.compositeAnyOf`,
rules as `TradeStrategy(openRules = …, closeRules = …)`. Give it a name not already in the file;
hand-built strategies take a bare name (`s6`), and the `_optimized` suffix is left alone as a marker
of the old GA lineage.

Write the comment header at the same time, leaving the metrics lines until step 5. The comment is
the only record of why the parameters are what they are — follow s6's, which is the format: thesis,
then each departure from its ancestor with the net figure that departure was worth, then how the
parameters were chosen, then the caveat.

### Step 4 — Register it

Add `"<name>" -> TestStrategy.<name>` to `BatchBacktester.strategies`, then format:

```bash
sbt -batch "backtest/scalafmt"
```

### Step 5 — Measure

```bash
sbt -batch "backtest/runMain currexx.backtest.BatchBacktester"
```

This is also the compile check — `backtest/Test/compile` can report success while producing nothing.
See Part 4 for reading the output.

**If it produced no trades**, the fault is almost always Part 1: a condition reading an unwritten
slot, an `allOf` of edges that never coincide, an inverted `LinesCrossing`, or two indicators
colliding on one slot. Check those before touching parameters.

### Step 6 — Ablate before believing it

A net figure says the combination worked, not which part of it did. Remove each rule and each filter
in turn, rerun, and record the delta. This is how s6's comment knows its squeeze is worth 5481 and
its inherited trend exit worth −824, and it is what makes the val maintainable: without it the next
reader cannot tell a load-bearing condition from decoration.

Ablation is also the diagnosis when a val loses money. If one condition carries everything, or the
family loses on the holdout regardless of parameters, the rules are the problem and no amount of
parameter tuning will fix them. s12 is the recorded case: searchable, thoroughly searched, and still
unprofitable, with a comment that says "needs rule redesign, not more GA".

### Step 7 — Decide

Part 4. A new val that loses on the holdout is not kept just for being new.

## Part 3 — Tuning an existing strategy

### Express the sweep as several vals in one run

`BatchBacktester` prints one line per registered strategy, over the same four corpora, in one run.
So a parameter sweep is not a loop of runs — it is one run over several vals that differ in the one
parameter:

1. Copy the base val once per point in the sweep, naming them so the varying parameter is obvious
   (`s6_mult24`, `s6_mult25`, `s6_mult26`). Change **one** parameter; copy the `rules` block verbatim.
2. Register all of them in `BatchBacktester.strategies`, keeping the base val in the list as the
   control.
3. Run once. The output section is the sweep table.
4. Delete every val that lost, keep at most the winner, and fold the finding into the survivor's
   comment.

While iterating, trim `BatchBacktester.strategies` down to the family under test — the run is four
corpora × six pairs per strategy, so runtime is roughly linear in the number registered. Restore the
full list before the run whose numbers get written into comments.

### Change one thing at a time, and sweep around the answer

s6 is the worked example of this whole section, and its comment is the format to reproduce: a coarse
grid picked the parameters, then a **27-point one-at-a-time perturbation sweep** around them
established which were structural and which were fitted. Trend lengths 70, 80, 85, 95, 100 and 110
all returned between 6289 and 7269 — a smooth ridge, so the trend length is structural. The band
multiplier was a spike: 2.6 earns 7362, 2.5 earns 4635, 2.4 earns 3863. The comment says outright to
treat 2.6 as fitted and the rest as structural, and that sentence is worth more than the number.

A parameter that only works at one value is a parameter that will not survive the next year of data.
Say so in the comment rather than quietly keeping the peak.

### Score the two searched years separately

s6's grid required **both** searched years to be profitable, scored separately rather than pooled.
That is the discipline that makes a manual sweep something other than curve-fitting: pooling lets a
strong year pay for a weak one, and `BatchBacktester` prints the two years separately precisely so
that it cannot. Only four other vals clear that bar.

### Keep parameters inside the ranges the repo already knows are sane

`GeneBounds` records the range each parameter is meaningful over — a lookback outside it is not
forbidden, it is untested and usually degenerate.

| Parameter | Range |
|---|---|
| Moving-average and standard-deviation lengths | 5..100 |
| Oscillator lengths (RSX, JRSX, STOCH, ATR, WilliamsR) | 5..50 |
| JMA length 5..100, phase −100..100, power 1..10 | |
| Indicator-level `atrLength`, `stdDevLength` | 5..50 |
| ADX 7..50, CCI 10..50, Ichimoku 9..52, CMF 10..40, NMA length and signal length 5..50 | |
| `keltnerMultiplier` 0.5..5.0, `bollingerMultiplier` 1.0..4.0, `nmaLambda` 0.5..4.0 | |

`IndicatorBounds` records four ratios that must hold *between* parameters, with the measured valid
bands. These are not style preferences; each was added after a violation produced a broken strategy.

| Relation | Ratio | Valid band | What breaks outside it |
|---|---|---|---|
| `volatilityRegime` | smoothing ÷ ATR length | 0.70..5.0 | smoothing shorter than the ATR it smooths inverts the regime every rule reads |
| `linesSeparation` | slow ÷ fast line | 1.20..5.0 | near-equal lines cross on noise and trade constantly for nothing |
| `keltnerAtr` | ATR ÷ middle band | 0.30..1.50 | an ATR over a longer window than its band measures a different market |
| `bollingerStdDev` | deviation window ÷ middle band | 0.45..2.50 | the deviation stops describing the stretch the band averages |

`ThresholdBounds` gives a `ThresholdCrossing`'s boundaries in the units of the transformation feeding
them: percentage for RSX, JRSX, STOCH and ADX; −100..0 for WilliamsR; ±250 for CCI; ±1 for CMF. A
boundary in the wrong units is a detector that never fires — CMF against a threshold of 50 leaves
`Condition.thresholdCrossing` with no reachable branch, so no signal, no trade, and a net of zero.

### Know what a good result looks like before you start

`ScoringFunction.Consistent.Config` is this repo's own written-down acceptance bar. It is not part of
the manual loop, but it is the list of thresholds the catalogue was built against, and it is worth
checking a candidate against:

- profit factor ≥ 1.2, and pair-month profit factor ≥ 1.3
- more than 55% of pair-months profitable, and at least two thirds of the six pairs profitable
- at least 5 closed trades per pair-month
- costs at most 40% of gross profit
- max drawdown at most 15%
- positive expectancy, and a positive *median* month — a strategy whose typical month loses money has
  not found an edge, however good the total reads

## Part 4 — Measuring and deciding

```bash
sbt -batch "backtest/runMain currexx.backtest.BatchBacktester"          # rank every registered val
sbt -batch "backtest/runMain currexx.backtest.StrategyAnalyser s6"      # explain one; no args = all
```

`StrategyAnalyser` takes keys of `BatchBacktester.strategies` and reports only over `majors1h`
(2024-07..2025-07), which is in sample for everything. It explains *how* a result was earned; it does
not say whether the strategy generalises. `BatchBacktester` is the one that does.

`BatchBacktester` prints four sections, one line per strategy:

```
--- holdout 2025-12..2026-06 (7 months, never selected) ---
<name>  net=…  closed=…  forced=…  win=…%  exp=…  PF=…  DD=…%  Sharpe=…  gross=…  costs=…
```

| Section | What it is |
|---|---|
| `searched 2023-07..2024-06` | first in-sample year, the one most of the file loses money in |
| `searched 2024-07..2025-07` | second in-sample year |
| `searched 2023-07..2025-07` | both pooled — for anything tuned, this is fit to the data that tuned it |
| `holdout 2025-12..2026-06` | seven months nothing has ever been selected against |

**The holdout line is the one that means anything.** Its net figures cover seven months against the
searched column's twenty-four, so they rank strategies against each other and are not a forecast. The
two searched years are reported separately because the pooled figure hides which of them paid for the
other. Note that `majors1h_202507_202606` is *not* a clean test set: its first months were used for
selection historically, which is why the holdout starts at 2025-12.

Record two lines per val in its comment, `searched 2023-07..2025-07` and `holdout 2025-12..2026-06`,
dropping the name, `gross` and `costs` columns; add the `searched 2023-07..2024-06` line where that
split matters to the decision. Note the double space after `holdout …:` that aligns the two. Do not
invent metrics — these figures come from `BatchBacktester` and nowhere else, and older vals carry
pre-cost-model numbers explicitly marked as not comparable.

The cost model is real: a 0.8 pip spread plus 0.1 pip slippage per side — one pip per round trip,
charged once per completed trade at the exit price — on 0.1 lots against a 10,000 balance. It is why
a variant that trades more often can look better on `gross` and worse on `net`.

### Reading it honestly

`StrategyAnalyser` exists because the one-line summary hides how a net figure was earned. Its sections
each answer a way it can mislead: `breakEvenWin` collapses win rate and payoff into the one number
that compares strategies whose win rates differ; `concentration` says whether removing the best five
trades removes the edge; `forced` says how much of the net is positions liquidated at the final mark
rather than closed by a rule; `monthly` and `per pair` say whether it worked in one regime or on one
pair; `by side` says whether it was a directional bet wearing a strategy's clothes.

Four questions decide whether a val is kept:

1. Is holdout net positive, and holdout profit factor above 1?
2. Does it beat the val it came from *on the holdout*? In-sample improvement is not evidence.
3. Is the holdout profit factor close to or above the in-sample one? A wide gap the wrong way means
   fitted. `s4_optimized_v2` is the anti-overfit example — 1.155 in sample, 1.565 out — and
   `s12_optimized` the opposite, 1.400 against 0.809.
4. Does `StrategyAnalyser` show the net spread across months, pairs and both sides?

Keep it, or delete it and say why. `TestStrategy` keeps every val a decision was ever based on, so a
report filename still resolves to something; `BatchBacktester` holds only the ones worth the runtime,
and a val dropped from it carries a `Not in BatchBacktester` line naming the val that dominates it.

### In-sample rank and holdout rank disagree, routinely

This is the most reliable finding the repo has produced, and it is why every decision is made on the
holdout column. `s5_optimized_v2` came ninth of the fourteen candidates it was measured against and
breached five of the acceptance thresholds above, yet posts the best holdout profit factor (2.079)
and Sharpe (4.131) in the catalogue. s6 leads its searched years and earns 74 per month out of
sample against 307 in. State that gap plainly when it appears rather than smoothing it over.

## Traps already paid for

- **A second oscillator eating the momentum zone.** s12's ADX threshold overwrote the CMF signal its
  rules read, and was never consumed as a filter. Use `ValueTracking(TrendStrength)` + `ValueIs`.
- **Thresholds in the wrong units.** CMF is bounded by ±1, so a threshold of 50 has no reachable
  branch in `Condition.thresholdCrossing` and the detector never fires.
- **Smoothing shorter than the ATR it smooths**, which inverts the regime: an indicator drawn at
  `(29, SMA(6))` closed 49 trades where the catalogue's own squeezes sit between 1.2 and 4 times.
- **A strategy that loses in 2023-07..2024-06.** Every JMA-crossover val does. It is a real regime and
  not a data problem, which is why the year is scored on its own rather than pooled away.
- **Tuning against the holdout.** It is worth something only for as long as nothing has been chosen
  against it. Sweep on the searched years; visit the holdout once, at the end, to decide.
