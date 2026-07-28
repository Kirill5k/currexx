---
name: promote-ga-champions
description: Promote the top result of every GA optimisation run in modules/backtest/optimisation-results into TestStrategy.scala and BatchBacktester.scala, then measure it. Use when asked to "add the optimisation results", "promote the champions", "pull in the latest GA runs", or after an Optimiser run finishes.
---

# Promote GA Champions

Turns each markdown report in `modules/backtest/optimisation-results/` into a compiling
`TestStrategy` val plus a `BatchBacktester` entry, so the winning parameter sets can be
compared against the existing strategies.

Paths

| Role | Path |
|---|---|
| Reports | `modules/backtest/optimisation-results/*.md` |
| Strategy catalogue | `modules/backtest/src/main/scala/currexx/backtest/TestStrategy.scala` |
| Batch runner | `modules/backtest/src/main/scala/currexx/backtest/BatchBacktester.scala` |
| Round definitions | `modules/backtest/src/main/scala/currexx/backtest/Optimiser.scala` |
| Type definitions | `modules/domain/src/main/scala/currexx/domain/signal/Indicator.scala` |

## Step 1 — Collect the reports

```bash
ls modules/backtest/optimisation-results/*.md
```

Read the whole of `TestStrategy.scala` and `BatchBacktester.scala` before editing anything —
you need the existing val names, their indicator params, and the base rule blocks.

## Step 2 — Extract the top result from each report

Every report has this shape:

```
# Genetic Algorithm Run: <label>
**Started at:** <iso timestamp>
**Target:** <indicator toString of the strategy that was optimised>
**Parameters:** GA(250,350,0.7,0.08,0.025,<shuffle>)
## Progress
### Generation N out of M   (top 3 members each)
**Top 25 members:**            (final shortlist, best first)
**Stats:** / **Duration:**
## Champion: <label>          (fitness, constraint verdict, indicator)
```

Pull the header and champion block:

```bash
f=modules/backtest/optimisation-results/<file>.md
grep -m1 '^\*\*Target:\*\*' "$f"
grep -m1 '^\*\*Parameters:\*\*' "$f"
awk '/^## Champion/{p=1} p' "$f"
```

Take the top result in this order of preference:

1. The `## Champion:` block — gives `Fitness: X.XXXXXX`, a constraint verdict, and `Indicator: …`.
2. If there is no Champion block (i.e. interrupted run), fall back to `#1` under the final `**Top 25 members:**` or `Generation X out of Y`, whose fitness is the
   number before the dash.

**Constraint verdict.** `Satisfies every constraint.` is the clean case. If it instead says
`BREACHES n constraint(s) despite winning:` the fitness only *discounted* the breach — still
promote the champion (it is the run's top result) but reproduce each breach line in the val's
comment so the next reader sees it without reopening the report.

## Step 3 — Find the base strategy and pick the name

The champion is the same *rules* as the strategy that was optimised, with different *indicator*
params. Identify that base strategy by matching the report's `**Target:**` string against the
`indicator` of each existing `TestStrategy` val; `Optimiser.scala`'s `rounds` list is the
cross-check (`name` → `strategy`). A report label ending in `_shuffle` is the shuffled GA run of
the same base val — the `_shuffle` suffix is a round label, never a val name.

Naming rule, derived from the existing catalogue (`s1_v2` → `s1_v2_optimized` → `s1_v2_optimized_v2`):

- base has no `_optimized` → `<base>_optimized`
- base ends `_optimized` → `<base>_v2`
- base ends `_optimized_vN` → `<base minus vN>_v{N+1}`

Never reuse a name that already exists in `TestStrategy.scala`. When several champions share one
base (typically a run and its `_shuffle` twin), sort them by **descending fitness** and allocate
consecutive versions, so the better champion gets the lower number.

**Skip duplicates.** If the champion's indicator is param-for-param identical to an existing val's
indicator, the run found nothing new — do not add a val, and note the skip in the final report.

## Step 4 — Translate the indicator string into Scala

The report prints case-class `toString`, i.e. positional args with no names. Convert to the named
form used throughout `TestStrategy.scala`. Parameter names and order come from
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
  // <report-file-name> (fitness X.XXXXXX, shuffled GA).      // ", shuffled GA" only when GA(...,true)
  // <metrics line from Step 8>
  val <new_name> = TestStrategy(
```

Use `Best Top-25 member from <file> (fitness …)` instead of `Champion from …` when the fitness
came from the Step 2 fallback. For a breaching champion, append the breach lines:

```scala
  // BREACHES 2 constraint(s) despite winning:
  //   - <breach text copied from the report>
```

Do not invent a `median win-to-loss ratio: …` or any other line. Older vals might carry different format, but no current
tool emits those numbers — the metrics line comes from `BatchBacktester` (Step 8).

## Step 6 — Register in BatchBacktester.scala

Add each new strategy to the `strategies` list as a new group at the end, below the existing entries,
so the batch report ends with new champions. Keep the blank-line grouping of the existing
entries untouched:

```scala
  val strategies: List[(String, TestStrategy)] = List(
    "s1_v2_optimized_v2" -> TestStrategy.s1_v2_optimized_v2,
    
    "<new_name>" -> TestStrategy.<new_name>
    …
```

The string key must equal the val name — it is the label in the results table.

## Step 7 — Format

```bash
sbt -batch "backtest/scalafmt"
```

This realigns the `->` arrows in `BatchBacktester.scala` and the named args in
`TestStrategy.scala` to the repo's `defaultWithAlign` style, `maxColumn = 140`.

## Step 8 — Measure, then backfill the metrics comment

Running the batch is also the compile check — `backtest/Test/compile` in this repo can report
success while producing nothing, so do not rely on it.

```bash
sbt -batch "backtest/runMain currexx.backtest.BatchBacktester"
```

Expect several minutes; run it in the background and let the completion notification come back.
Fix any compile error and rerun before reading results.

Each new strategy gets one output line:

```
<name>  net=   0.99721  closed=  196  forced= 1  win= 44.90%  exp= 0.005088  PF=  2.104  DD=  6.12%  Sharpe=  1.883  gross=   1.10412  costs=  0.10691
```

Write it back into that val's comment header (Step 5) as the metrics line, dropping the name column:

```scala
  // net=0.99721, closed=196, forced=1, win=44.90%, exp=0.005088, PF=2.104, DD=6.12%, Sharpe=1.883, gross=1.10412, costs=0.10691
```

Then re-run `sbt -batch "backtest/scalafmt"` if any comment pushed a line past 140 columns.

## Step 9 — Report

Tell the user, per report file:

- new val name, its base strategy, fitness, and whether it came from a shuffled run
- the measured `net` / `closed` / `win` / `PF` / `DD` / `Sharpe`, and how that compares to the base
  strategy's line in the same batch run
- anything skipped — duplicate params, interrupted report, or a champion that breaches constraints

State plainly if a champion measured *worse* than the base it was optimised from; a high GA fitness
with weak batch metrics is the signal worth surfacing, not something to smooth over.
