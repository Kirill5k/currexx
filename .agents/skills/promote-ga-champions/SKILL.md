---
name: promote-ga-champions
description: Read GA and SCGA optimisation reports, add candidates to TestStrategy and StrategyCatalogue, and compare their diagnostics and continuous batch results. Use when asked to promote champions or add completed optimisation results to the strategy catalogue.
---

# Promote GA Champions

Turn optimisation reports into named, measured strategy candidates. Preserve the run's
selection and distinguish selected champions from candidates retained only for reference.
Use the diagnostics to explain what the search achieved; use `BatchBacktester` to measure
continuous performance before deciding what to retain within the requested promotion scope.

| Role | Path |
|---|---|
| Reports | `optimisation-results/*.md` |
| Strategy definitions and lineage | `modules/backtest/src/main/scala/currexx/backtest/TestStrategy.scala` |
| Named registry and batch membership | `modules/backtest/src/main/scala/currexx/backtest/StrategyCatalogue.scala` |
| Batch runner and corpus labels | `modules/backtest/src/main/scala/currexx/backtest/BatchBacktester.scala` |
| Round definitions and named seeds | `modules/backtest/src/main/scala/currexx/backtest/Optimiser.scala` |
| Corpora / folds | `modules/backtest/src/main/scala/currexx/backtest/MarketDataProvider.scala` |
| Indicator types | `modules/domain/src/main/scala/currexx/domain/signal/Indicator.scala` |
| Report semantics | `modules/backtest/README.md` |

## Step 1 — Collect the reports

```bash
rg --files optimisation-results -g '*.md'
```

Reports are retained. Identify new ones by filename timestamp, Git status, and filenames
already cited in strategy comments. Include both GA and SCGA reports.

Read `TestStrategy.scala`, including its object-level documentation, `StrategyCatalogue.scala`,
`BatchBacktester.scala`, and the relevant rounds before editing. The round configuration may
have changed since a historical report; use its recorded target and cited lineage to cross-check.

## Step 2 — Identify the selected candidate, then read its diagnostics

Current reports contain these sections:

```text
# <algorithm display name> Run: <round name>
**Target:** <indicator>
**Parameters:** GA(...) or SCGA(...)
## Progress
### Generation N out of M
## Stable progress: generation N
## Final Results
## Champion selection: <round name>
## Baseline measurements
## Baseline comparisons
## Leader fold diagnostics
## Finalist provenance
## Search observations and workload
```

Older reports may lack the diagnostic sections. Treat absent measurements as unavailable.
Do not rerun an optimisation just to fill them in.

**Scope extraction to its section.** Several sections contain `Indicator:` lines: the selected
candidate, shortlist training leader, and best candidate ever searched can differ. Older
reports call the shortlist training leader `Final training leader`. Never choose the
last `Indicator:` in the file or replace the champion with the largest score found elsewhere.

The `Final Results` columns are `rank train# training validation retained individual`.
Training is rescored on all search folds. The table preserves the validator's order: positive
validation gates eligibility; candidates within the reported tie band of the best validation
score are ordered by training, then the remaining survivors by validation. The current default
band is 5%; do not re-sort the shortlist by validation alone. `train#` ranks finalists by
training; it is not a rank among every candidate the run ever evaluated.

Current runs reserve shortlist places for the target and distinct compatible effective
seeds, then consider the final population and a bounded all-fold search archive. GA fills
remaining places by training fitness; SCGA also preserves unrepresented species where
capacity permits. The archive has the same capacity as the shortlist and does not feed
candidates back into evolution. A shortlisted candidate need not have survived in the final
population. These selection changes do not apply retroactively to older reports.

Use this precedence:

- **`SELECTED (from N after validation, ties inside ... broken on training): ...`** identifies
  the selected champion. Older `SELECTED (best of N on validation): ...` wording is also valid
  for its report. Read the indicator from that champion block and cross-check final rank 1.
  Copy any `BREACHES` lines: a breach can discount fitness without disqualifying the candidate.
  The optional `Selection source:` line distinguishes a retained target, supplied seed
  parameters, and a searched candidate. A selected seed may improve on the target, but its
  parameters are evaluated under the current round's rules; check duplicates before adding
  anything. Target retention does not establish a newly discovered strategy.
- **`NOTHING SELECTED: ...`** means no champion cleared the configured gate, or validation was
  unavailable. The leading finalist can still be added for reference under the existing
  workflow, but label it **diagnostic only**, not selected or validation-approved. If the user
  requested selected champions only, omit reference entries.
- **Complete `Final Results` with missing or incomplete diagnostics** records the completed
  search and finalist ordering. An `Incomplete diagnostics: ...` note means replay or output
  failed afterward. Use the champion block if present; otherwise label final rank 1 a
  **completed-finalist candidate**, with selection and constraint verdicts unavailable.
  Record the incomplete diagnostics and preserve available scores; do not invent a verdict.
  Later rounds may have completed normally. A missing champion block alone does not establish
  that a run was interrupted.
- **No complete `Final Results`**: the last `Top Members` rank 1 can be retained as an
  **unvalidated reference**, following the existing fallback workflow. Its rotating-fold score
  is not full-search fitness. Cite the generation and write `Progress leader from ...` rather
  than `Champion from ...`. If there is no candidate, there is nothing to add.

A completed but empty shortlist supplies no candidate; do not substitute an earlier progress
leader. With zero training fitness, retention is unavailable. Preserve the absolute scores
without inventing a percentage or treating validation alone as proof of general improvement.

Read the diagnostics with these meanings:

| Section | What to extract and how to interpret it |
|---|---|
| Baseline measurements | Target and named seed training/validation scores, accepted or incompatible status, and aliases after fixed inputs are restored. Seed parameters are evaluated under this round's rules, not the seed strategy's original rules. |
| Baseline comparisons | Per-fold net, trade, forced-closure, cost, drawdown, and breach differences against the target; fitness differences against the target and strongest seed for each metric. Report training and validation disagreements separately. A zero baseline has no percentage improvement. |
| Leader fold diagnostics | Measurements for the leading finalist, distinct shortlist training leader (called `Final training leader` in older reports), and distinct best-seen candidate. The training leader may originate from the archive or protected baselines. Each fold resets state and liquidates remaining positions at its end; these are segment results, not continuous multi-year net. |
| Finalist provenance | First successful search evaluation, including generation 0, plus baseline and catalogue matches. Validation, rescoring, and reporting replays do not establish a discovery generation. A baseline first evaluated during final assembly has `first seen=not observed`. |
| Search observations and workload | Best all-fold fitness ever searched, whether it reached the shortlist, requests, cache computations/reuse, actual simulations, and separate optimisation/reporting durations. Final rescore requests include missing baseline evaluations. Search + rescore requests include cache hits and in-flight waiters; requests are not independent candidates or simulations. |

`Stable progress` uses all-fold fitness and can be compared across generations. Ordinary
`Top Members` scores use rotating folds and cannot. The best-seen candidate may have been
lost from the population and considered through the archive, but archive membership alone
does not guarantee shortlisting or selection. Its later diagnostic replay does not select
it. Do not add it as an extra champion just because the report now exposes it.

## Step 3 — Find the base strategy, detect duplicates, and pick a name

Candidates inherit the optimised base's **rules**, with the effective indicator parameters
printed in the report. Resolve the base from the round configuration and recorded target;
matching target indicators alone cannot distinguish strategies with different rules. A
`_shuffle` suffix is a round label, not a strategy name. Read the recorded GA/SCGA parameters
for shuffle status rather than inferring it from the name. Decode positional fields using
`Parameters` in `modules/algorithms/src/main/scala/currexx/algorithms/Algorithm.scala` when needed.

**Skip actual strategy duplicates:** both stored indicators and trading rules must match.
Cross-check report matches against the current source, which may have changed since the run.

- `catalogue strategies=... (exact)` identifies matching rules and parameters at report time.
- `after restoring fixed inputs` identifies equivalence within that round. Check the actual
  stored indicator before skipping: its original fixed values may differ from the candidate.
- `parameter-only catalogue matches (different rules)` is not a duplicate strategy.
- Target/seed aliases show repeated effective parameters under the round's rules. They can
  explain a lack of search novelty without establishing equality with a stored seed strategy.

Suffixes do not rank candidates or necessarily run contiguously. Pick an unused name following
this family's existing convention, keep related definitions adjacent, and preserve existing
names/order. When several runs share a base, use their reported validation results to allocate
new names consistently; this naming step does not change selection within any report.

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

Copy the resolved base's entire `rules = TradeStrategy(...)` block, including comments.
Translate only the effective indicator parameters. Insert the new definition after its base
or the last related definition.

Use the report filename as provenance and retain the distinction between a selected champion,
a completed-finalist candidate with missing diagnostics, a diagnostic-only finalist, and an
unvalidated progress reference. For the missing-diagnostics case, write `Completed finalist
from ... (diagnostics incomplete; selection and constraint verdicts unavailable)`. For a
selected champion, for example:

```scala
  // GA-optimized indicator params for <base> (rules unchanged). Champion from
  // <report-file-name> (training X.XXXXXX -> validation Y.YYYYYY, retaining Z.Z%).
  // Satisfies every constraint on validation data.
  // searched <actual period>: <continuous batch metrics from Step 8>
  // historical <actual period>: <continuous batch metrics from Step 8; record prior reuse>
  val <new_name> = TestStrategy(
```

Use `SCGA-optimized` and the actual shuffle status where applicable. Replace the constraint
line with the reported breach lines, or mark the constraint diagnostics unavailable. Do not
infer that constraints passed from a positive score. For zero training fitness, retain `n/a`
and explain why. If the base was later deleted, say so in the provenance comment.

Summarise relevant baseline deltas and whether the candidate was already a target/seed, while
keeping those fold measurements clearly labelled. Continuous `net`/`closed`/`win`/`PF` metrics
in the batch-period comments come from Step 8, not sums of report folds. Older pre-cost-model
measurements are not comparable to current accounting.

## Step 6 — Register in StrategyCatalogue.scala

Append new candidates to `StrategyCatalogue.entries`, preserving the existing relative order:

```scala
Entry("<new_name>", TestStrategy.<new_name>, includeInBatch = true)
```

The key must equal the val name. Every public `TestStrategy` definition belongs in the registry;
lineage-only entries use `includeInBatch = false`. New candidates being measured use `true`.
`BatchBacktester.strategies` already delegates to `StrategyCatalogue.batch`; do not recreate a
second registration list in the runner.

Update the explicit batch-order expectations in `StrategyCatalogueSpec` for deliberate
membership changes. Its completeness check discovers public strategy definitions and catches
forgotten registrations. If the task later renames, deletes, or moves a candidate to lineage,
keep the registry, tests, round/seed references, and strategy documentation consistent.

## Step 7 — Format and verify registration

```bash
sbt --batch ';backtest/scalafmt;backtest/testOnly *StrategyCatalogueSpec;backtest/compile'
```

Use the repository formatter and a real compile check before the expensive batch. The
catalogue test checks registration and ordering; it does not measure trading performance.

## Step 8 — Measure, then backfill the metrics comments

```bash
sbt --batch 'backtest/runMain currexx.backtest.BatchBacktester'
```

Run the configured batch to completion and inspect its actual output. It currently contains
four sections, with one line per included strategy:

```text
--- searched 2023-07..2024-06 (12 months, in sample) ---
--- searched 2024-07..2025-07 (12 months, in sample) ---
--- searched 2023-07..2025-07 (24 months, in sample) ---
--- historical 2025-12..2026-06 (7 months, reused for s10_v2 development) ---
<name> net=... closed=... forced=... win=... exp=... PF=... DD=... Sharpe=... gross=... costs=...
```

Use the configured dates and labels if they change. Backfill continuous combined-search and
later-period `net`, `closed`, `forced`, `win`, `exp`, `PF`, `DD`, and `Sharpe` into each new val's
comments. Use the separate searched-year results to explain regime differences in the final
comparison. Preserve cost information when it affects the conclusion.

The currently named `majors1hHoldout` is labelled **historical** by the runner: it has been reused
for development and promotion. Do not call it untouched, never selected, or independent
confirmation. Validation is also used to choose finalists. Report improvements on each period
separately and compare candidates with their base under the same batch settings; unequal
period lengths do not justify comparing raw net totals as forecasts or equal-duration returns.

These continuous runs remain necessary even when report fold net deltas are available. Record
missing or failed measurements as such, never as zero. Format again after updating comments.

## Step 9 — Report the result

For each source report, state:

- Candidate name and base, GA/SCGA and shuffle status, selected/reference status, training and
  validation scores, and known breaches or incomplete diagnostics.
- Whether the candidate improved on the target and strongest seed on training and validation;
  use fold net/activity differences to qualify fitness gains rather than declaring a universal
  winner from fitness alone.
- Discovery generation, target/seed/catalogue matches, and whether the best-seen candidate was
  lost from the shortlist, where these explain the run's result.
- Actual search/rescore work and additional reporting work/time when available. Do not count
  reporting replays as optimisation evaluations or independent evidence.
- Continuous batch results against the base on each relevant period, including disagreements
  between searched years, validation, and the later reused historical period.
- What was added, retained for reference, skipped as an actual duplicate, renamed, or removed
  within the requested scope, with the source report filename for each decision.

State plainly when a candidate is worse than its base on a measured period or when improved
fitness did not improve net results. Diagnostic visibility does not establish fresh
out-of-sample performance.
