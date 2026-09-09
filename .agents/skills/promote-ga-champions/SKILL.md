---
name: promote-ga-champions
description: Promote the selected champion of every GA optimisation run in optimisation-results into TestStrategy.scala and BatchBacktester.scala, measure it on the searched and holdout corpora, then keep or discard it on the holdout result. Use when asked to "add the optimisation results", "promote the champions", "pull in the latest GA runs", or after an Optimiser run finishes.
---

# Promote GA Champions

Turns each markdown report in `optimisation-results/` into a compiling
`TestStrategy` val plus a `BatchBacktester` entry, measures it, and then decides whether it
earns a place in the catalogue.

Promotion is not the end of the job. A champion is a candidate until `BatchBacktester` has
scored it on the holdout corpus; the last steps keep the winners, delete the rest, and leave
the catalogue's own documentation true.

Paths

| Role | Path |
|---|---|
| Reports | `optimisation-results/*.md` |
| Strategy catalogue | `modules/backtest/src/main/scala/currexx/backtest/TestStrategy.scala` |
| Batch runner | `modules/backtest/src/main/scala/currexx/backtest/BatchBacktester.scala` |
| Round definitions | `modules/backtest/src/main/scala/currexx/backtest/Optimiser.scala` |
| Corpora / folds | `modules/backtest/src/main/scala/currexx/backtest/MarketDataProvider.scala` |
| Type definitions | `modules/domain/src/main/scala/currexx/domain/signal/Indicator.scala` |

## Step 1 — Collect the reports

```bash
ls optimisation-results/*.md
```

Reports are never deleted, so work out which are new — by timestamp in the filename, by `git
status`, or by which report filenames the existing `TestStrategy` comments already cite.

Read the whole of `TestStrategy.scala` (including its object-level scaladoc, which is the
catalogue's own account of itself), `BatchBacktester.scala` and `Optimiser.scala`'s `rounds`
before editing anything.

## Step 2 — Read the champion selection block

A report has this shape:

```
# Genetic Algorithm Run: <round name>
**Started at:** <iso timestamp>
**Target:** <indicator toString of the strategy that was optimised>
**Parameters:** GA(<pop>,<maxGen>,<crossover>,<mutation>,<elitism>,<shuffle>,<initialOversampling>)
## Progress
### Generation N out of M          **Top Members:** — top 3, ranked on TRAINING only
## Final Results                   narrative + validation-ranked shortlist table + Stats + Duration
## Champion selection: <round name>   corpus description, verdict, Indicator
```

```bash
f=optimisation-results/<file>.md
grep -m1 '^\*\*Target:\*\*' "$f"
grep -m1 '^\*\*Parameters:\*\*' "$f"
awk '/^## Final Results/{p=1} p' "$f"
```

### Two scores, not one

Every candidate carries a **training** score from the search folds and a **validation** score
from a fold the search never touched. The `## Final Results` shortlist is ranked on
*validation*, and `## Champion selection` reports the top of that ranking — so nothing here
chooses anything, and the shortlist's `#1` and the champion are the same individual.

The shortlist table's columns are `rank train# training validation retained individual`;
`train#` is where that individual placed on training, so a champion with `train# 13` means the
training-ranked winner was not the one that survived validation.

### The verdict decides whether there is anything to promote

`## Champion selection` ends in one of two ways.

**`SELECTED (best of N on validation): training X -> validation Y, retaining Z%`** — a
candidate. Followed by either `Satisfies every constraint on validation data.` or `BREACHES n
constraint(s) on validation data:` with one line per breach. A breach discounted the fitness
rather than disqualifying it, so still promote — but reproduce every breach line in the val's
comment so the next reader sees it without reopening the report.

**`NOTHING SELECTED: no finalist scored above zero on data it was never searched against.`** — 
promote top candidate for reference. Make sure it is reflected in val's comment.

### Two readings that need flagging, not skipping

- **`training 0.000000`** (and therefore `retaining n/a`): the search folds scored the champion
  at zero and the single validation fold ranked it alone. Promote it, and say so in the comment
  — a validation figure from such a round filters out the hopeless, it does not rank the rest.
- **No `## Champion selection` block at all** (interrupted run): fall back to `#1` of the last
  `**Top Members:**` block, which is *training*-ranked and never validated. Say so in the
  comment — write `Best Top-25 member from …` instead of `Champion from …` — and expect it to
  measure badly.

## Step 3 — Find the base strategy and pick a name

The champion is the same *rules* as the strategy that was optimised, with different *indicator*
params. Identify that base by matching the report's `**Target:**` against the `indicator` of
each `TestStrategy` val; `Optimiser.scala`'s `rounds` list is the cross-check (`name` →
`strategy`). A round name ending in `_shuffle` is the shuffled GA run of the same base val — a
round label, never a val name.

**Suffixes carry no meaning.** They neither rank a family nor run contiguously within one
(`s5_optimized_v2` has no `s5_optimized` above it; `s2_optimized_v3` out-scores `s2_optimized`),
because Step 9 renames winners into their base's name and deletes what they beat. A suffix
records only that a val once needed distinguishing from something. So:

- pick any name not already in `TestStrategy.scala`, following the `<base>_optimized` /
  `<base>_vN` shape of the file
- when several champions share one base (typically a run and its `_shuffle` twin), order them by
  **descending validation** score and allocate consecutive versions
- do not renumber or reorder anything existing to make the new name fit

**Skip duplicates.** If the champion's indicator is param-for-param identical to an existing
val's, the run found nothing new — no val, and note the skip in the final report.

## Step 4 — Translate the indicator string into Scala

The report prints case-class `toString`, i.e. positional args with no names. Convert to the
named form used throughout `TestStrategy.scala`. Parameter names and order come from
`modules/domain/src/main/scala/currexx/domain/signal/Indicator.scala` — re-read it if a case is
not in the tables below.

`Indicator`:

| toString | Scala |
|---|---|
| `Composite(NonEmptyList(a, b, c),Any)` | `Indicator.compositeAnyOf(a, b, c)` |
| `Composite(NonEmptyList(a, b, c),All)` | `Indicator.compositeAllOf(a, b, c)` |
| `TrendChangeDetection(src,vt)` | `Indicator.TrendChangeDetection(source, transformation)` |
| `ThresholdCrossing(src,vt,u,l)` | `Indicator.ThresholdCrossing(source, transformation, upperBoundary, lowerBoundary)` |
| `LinesCrossing(src,vt1,vt2)` | `Indicator.LinesCrossing(source, line1Transformation, line2Transformation)` |
| `KeltnerChannel(src,vt,n,m)` | `Indicator.KeltnerChannel(source, middleBand, atrLength, atrMultiplier)` |
| `BollingerBands(src,vt,n,m)` | `Indicator.BollingerBands(source, middleBand, stdDevLength, stdDevMultiplier)` |
| `VolatilityRegimeDetection(n,vt)` | `Indicator.VolatilityRegimeDetection(atrLength, smoothingType)` |
| `ValueTracking(role,src,vt)` | `Indicator.ValueTracking(role, source, transformation)` |
| `PriceLineCrossing(src,role,vt)` | `Indicator.PriceLineCrossing(source, role, transformation)` |

`ValueTransformation` — all prefixed `ValueTransformation.`:

| toString | Named args |
|---|---|
| `SMA(n)` `EMA(n)` `WMA(n)` `HMA(n)` `ATR(n)` `RSX(n)` `JRSX(n)` `STOCH(n)` `ADX(n)` `WilliamsR(n)` `CCI(n)` `IchimokuKijunSen(n)` `CMF(n)` `StandardDeviation(n)` | `length = n` |
| `JMA(l,p,w)` | `length = l, phase = p, power = w` |
| `NMA(l,s,λ,ma)` | `length = l, signalLength = s, lambda = λ, maCalc = MovingAverage.<ma>` |
| `Kalman(g,m)` `KalmanVelocity(g,m)` | `gain = g, measurementNoise = m` |
| `ParabolicSAR(a,b,c)` | `afStart = a, afMax = b, afStep = c` |
| `Sequenced(List(a, b))` | `ValueTransformation.sequenced(a, b)` |

Enums: `HLC3`/`Close`/`Open`/`HL2` → `ValueSource.X`; `Momentum`/`Volatility`/`Velocity`/
`ChannelMiddleBand`/`TrendStrength`/`Price` → `ValueRole.X`; `Exponential`/`Simple`/`Weighted`/
`Hull` → `MovingAverage.X`.

## Step 5 — Add the val to TestStrategy.scala

Copy the base val's **entire `rules = TradeStrategy(...)` block verbatim**, inline comments
included — only the indicator params change. Insert the new val directly after the base val's
block (or after the last val already derived from that base) so related strategies stay adjacent.

Comment header, following the format already in the file:

```scala
  // GA-optimized indicator params for <base> (rules unchanged). Champion from
  // <report-file-name> (training X.XXXXXX -> validation Y.YYYYYY, retaining Z.Z%, shuffled GA).
  // Satisfies every constraint on validation data.
  // searched 2023-07..2025-07: <metrics from Step 8>
  // holdout 2025-12..2026-06:  <metrics from Step 8>
  val <new_name> = TestStrategy(
```

- `, shuffled GA` only when the sixth field of `**Parameters:**` is `true`. Reports written before 2026-09-01 have no seventh field, so
  that line ends in the boolean instead.
- `retaining Z.Z%` is copied from the verdict; omit it when the report says `n/a`, and add a
  sentence saying the training score was zero.
- Replace the `Satisfies` line with the breach block when the verdict breaches:

```scala
  // BREACHES 2 constraint(s) on validation data:
  //   - <breach text copied verbatim from the report>
```

- When the base named in the first line was itself deleted by a later prune, the file's phrasing
  is `for <base>, which is no longer in this catalogue (rules unchanged)`.
- Free prose between the verdict and the metrics lines is where the interesting reading goes —
  how it compares to its base, whether its shuffled twin found anything, whether holdout beat
  in-sample. Write it after Step 8, when there are numbers to write about.
- Do not invent metrics. `net`/`closed`/`win`/`PF` figures come from `BatchBacktester` and
  nowhere else; older vals carry pre-cost-model numbers, explicitly marked as not comparable.

## Step 6 — Register in BatchBacktester.scala

Add each new strategy to the `strategies` list as a new group at the end, below the existing
entries, keeping the blank-line grouping of the existing entries untouched:

```scala
  val strategies: List[(String, TestStrategy)] = List(
    "s1_v2_optimized" -> TestStrategy.s1_v2_optimized,
    …

    "<new_name>" -> TestStrategy.<new_name>
  )
```

The string key must equal the val name — it is the label in the results table. Not every val in
`TestStrategy.scala` is here: vals kept only for lineage are marked `Not in BatchBacktester.` in
their comment and stay out. New champions always go in; that is what Step 8 measures.

## Step 7 — Format

```bash
sbt -batch "backtest/scalafmt"
```

This realigns the `->` arrows in `BatchBacktester.scala` and the named args in
`TestStrategy.scala` to the repo's `defaultWithAlign` style, `maxColumn = 140`.

## Step 8 — Measure, then backfill the metrics comments

Running the batch is also the compile check — `backtest/Test/compile` in this repo can report
success while producing nothing, so do not rely on it.

```bash
sbt -batch "backtest/runMain currexx.backtest.BatchBacktester"
```

Expect a long run (three corpora × every strategy); run it in the background and let the
completion notification come back. Fix any compile error and rerun before reading results.

The output is three sections, each with one line per strategy:

```
--- majors 1h 2024-07..2025-07 (12 months, original sample) ---
--- searched 2023-07..2025-07 (24 months, in sample) ---
--- holdout 2025-12..2026-06 (7 months, never selected) ---
<name>  net=6285.64646  closed= 1259  forced=10  win= 45.75%  exp= 4.992571  PF=  1.259  DD=  1.79%  Sharpe=  1.597  gross=…  costs=…
```

Record **two** of the three per val — `searched` and `holdout` — dropping the name, `gross` and
`costs` columns:

```scala
  // searched 2023-07..2025-07: net=6285.64646, closed=1259, forced=10, win=45.75%, exp=4.992571, PF=1.259, DD=1.79%, Sharpe=1.597
  // holdout 2025-12..2026-06:  net=1535.84151, closed=382, forced=6, win=45.55%, exp=4.020528, PF=1.242, DD=0.85%, Sharpe=1.548
```

The first section is a subset of `searched` and is deliberately not recorded per val. Note the
double space after `holdout …:` that aligns the two lines.

**The holdout line is the one that means anything.** `searched` is the two years the GA folds
cover, so for anything `_optimized` it reports fit to the data that chose it. Holdout net
figures cover seven months against the searched column's twenty-four, so they rank strategies
against each other and are not a forecast.

Then re-run `sbt -batch "backtest/scalafmt"` if any comment pushed a line past 140 columns.

## Step 9 — Report

Tell the user, per report file:

- new val name, its base, `training -> validation` with the retention percentage, whether the
  run was shuffled, and the constraint verdict
- the measured holdout `net` / `closed` / `win` / `PF` / `DD` / `Sharpe`, and how that compares
  to the base's holdout line in the same batch run — plus the in-sample line where the two
  disagree
- what Step 9 did: kept, promoted into a base's name, or deleted
- anything skipped — `NOTHING SELECTED`, duplicate params, an interrupted report

State plainly when a champion measured *worse* than the base it was optimised from, and when a
high validation fitness produced weak holdout metrics. That gap is the signal worth surfacing,
not something to smooth over: the GA's own ranking has repeatedly disagreed with the holdout,
and the catalogue's best performers have come out of rounds with mediocre fitness and breached
constraints.
