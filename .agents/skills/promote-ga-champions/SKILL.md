---
name: promote-ga-champions
description: Read GA and SCGA optimisation reports, use their explicit upgrade decisions to select candidates for TestStrategy and StrategyCatalogue, and compare continuous batch results. Use when asked to promote champions or add completed optimisation results to the strategy catalogue, including legacy reports without upgrade decisions.
---

# Promote GA Champions

Turn optimisation reports into named, measured strategy candidates. For current reports,
use the explicit upgrade decision to identify the candidate that qualifies for further
evaluation. Search rank and positive validation fitness do not establish an upgrade.
Adding a catalogue candidate does not deploy a strategy. Use `BatchBacktester` to measure
continuous performance within the requested promotion scope.

| Role | Path |
|---|---|
| Reports | `optimisation-results/*.md` |
| Strategy definitions and lineage | `modules/backtest/src/main/scala/currexx/backtest/TestStrategy.scala` |
| Named registry and batch membership | `modules/backtest/src/main/scala/currexx/backtest/StrategyCatalogue.scala` |
| Batch runner and corpus labels | `modules/backtest/src/main/scala/currexx/backtest/BatchBacktester.scala` |
| Round definitions and named seeds | `modules/backtest/src/main/scala/currexx/backtest/OptimisationRounds.scala` |
| Corpora / folds | `modules/backtest/src/main/scala/currexx/backtest/MarketDataProvider.scala` |
| Indicator types | `modules/domain/src/main/scala/currexx/domain/signal/Indicator.scala` |
| Upgrade decisions and objective workflow | `docs/strategy-upgrades.md` |
| Report fields and decision rendering | `modules/backtest/src/main/scala/currexx/backtest/optimizer/reporting/UpgradeDecisionRenderer.scala` |
| Other report semantics | `modules/backtest/README.md` |

## Step 1 — Collect the reports

```bash
rg --files optimisation-results -g '*.md'
```

Reports are retained. Identify new ones by filename timestamp, Git status, and filenames
already cited in strategy comments. Include both GA and SCGA reports.

Read `TestStrategy.scala`, including its object-level documentation, `StrategyCatalogue.scala`,
`BatchBacktester.scala`, and the relevant rounds before editing. The round configuration may
have changed since a historical report; use its recorded target and cited lineage to cross-check.

## Step 2 — Read the upgrade decision, then its evidence

Identify the report format before selecting a candidate. Current reports use format version 2
and contain these sections:

```text
# <algorithm display name> Run: <round name>
**Target:** <indicator>
**Parameters:** GA(...) or SCGA(...)
## Progress
## Stable progress: generation N
## Final Results                         # Current objective
## Final rankings (separate objectives) # BaselineRelative objective; replaces Final Results
## Search ranking: <round name>
## Upgrade decision
## Baseline measurements
## Baseline comparisons
## Leader fold diagnostics
## Finalist provenance
## Search observations and workload
```

**Scope extraction to its section and candidate identity.** The `SEARCH LEADER`, leading
finalist, approved indicator, and best candidate ever searched can differ. Do not select the
last `Indicator:` line, final rank 1, or the largest score found elsewhere in the file.

For current reports, use the `Upgrade decision` section:

- **`UPGRADE APPROVED`**: take the candidate from `Approved indicator:` and cross-check the
  `candidate` in `Recorded decision:` JSON when present. Match that identity to the final
  rankings for its search and absolute validation scores. The policy scans the existing
  validation order, skips the target, and approves the first challenger that passes every
  requirement. A rejected leader can precede an approved challenger. Seeds are challengers
  under the target's rules. Preserve the recorded decision; do not re-sort, expand the
  shortlist, or run another search to find a different winner.
- **`BASE RETAINED`**: no assessed challenger qualifies. Do not create an upgraded strategy
  from the search leader or a positive-scoring finalist. Record the assessed rejections and
  skip adding a new upgrade. Add rejected candidates as **diagnostic-only references** only
  when the user's scope includes such references; never label them approved upgrades.
- **Missing, incomplete, or conflicting decision/identity**: mark the upgrade decision
  **unavailable**. Rankings and `Satisfies every constraint` do not substitute for it. A
  completed final table permits a **completed-finalist reference** within the requested
  reference scope, but it cannot establish approval.

A reporting failure cannot change the completed decision. If a clear decision and candidate
identity were written before an `Incomplete diagnostics` note, retain that decision and mark
missing evidence as unavailable. If the note says the decision completed but does not record
its outcome, do not infer the outcome. Final output can also fail before any complete ranking
is written, so missing output alone does not prove that search or validation failed. Do not
rerun optimisation solely to fill a report gap. A truncated current report remains a current
report; do not apply a legacy fallback to grant it approval.

The decision uses the selection period, which search does not use. Extract:

- `Search objective:` and `Upgrade policy:` JSON, including versions and full settings. The
  default policy is a research setting, not calibrated proof of reliability. Use the values
  recorded for that run; do not substitute today's defaults or tune thresholds after seeing
  results. See `docs/strategy-upgrades.md` for the trial formulas and usage workflow.
- `Selection baseline:` and comparison evidence: base identity, pair/date coverage, initial
  capital, net profit after costs, drawdown, costs, absolute score, and constraint violations.
- Required net improvement `H`, actual net improvement, drawdown increase, covered months,
  winning months, worst monthly net difference, stressed base/candidate net, and stressed
  improvement. Use the monthly differences and recorded limits for details absent from the
  summary table. Each assessed rejection has a stable reason code, measured value, and
  required value. Preserve these values before display rounding.

Cost sensitivity is a fixed-trade accounting check at the recorded cost multiplier. It does
not simulate changed fills or trading behaviour. Approval qualifies a candidate for further
evaluation; it does not establish future profit or authorize live deployment.

Search scores use the configured objective. `BaselineRelative` scores include the net-profit
multiplier for each search fold; validation ordering always uses the unchanged absolute
quality scorer. Preserve the actual reported scores and label both objectives. Omit the
validation/training retention percentage when the objectives differ. Under `Current`, that
ratio remains diagnostic and is unavailable when training fitness is zero. Neither a ratio
nor a fitness increase proves incremental net profit. For a partial current report with no
objective metadata, do not infer equal objectives from a heading or calculate a retention
ratio. A printed ratio can only be quoted as reported and unverified.

The final rankings preserve the validator's order, including its reported tie band. Positive
validation fitness is part of shortlist ordering, not the upgrade decision. Current runs
reserve places for the target and compatible effective seeds, then consider the final
population and a bounded all-fold archive. GA fills remaining places by search fitness;
SCGA also preserves unrepresented species where capacity permits. The archive does not feed
candidates back into evolution. Preserve these recorded roles when explaining the result.

### Legacy reports

Older reports use `Champion selection` and have no explicit upgrade policy decision. Keep
their historical meaning and files unchanged. Do not invent a version-2 decision from their
scores or apply today's policy retroactively.

- **`SELECTED (from N after validation, ties inside ... broken on training)`**, or the older
  **`SELECTED (best of N on validation)`**, identifies the legacy validation-selected champion.
  Read that block and cross-check final rank 1. Within a legacy promotion request, it can be
  added with status **legacy validation-selected; upgrade unassessed**. Copy reported breaches;
  a positive score did not prove all constraints passed or establish improvement over the base.
- **`NOTHING SELECTED`**: no legacy champion cleared its gate. A leading finalist can be kept
  as **diagnostic only** within reference scope. Omit it for selected-champions-only requests.
- **Complete `Final Results`, absent selection diagnostics**: preserve a
  **completed-finalist reference; selection and upgrade verdicts unavailable** within scope.
- **Only progress output**: the last `Top Members` rank 1 can be an **unvalidated progress
  reference** within scope. Cite its generation and rotating-fold score, not full-search
  fitness. A completed but empty shortlist supplies no candidate; do not substitute progress.

For requests limited to explicitly approved upgrades, legacy reports supply none. In legacy
`Final Results`, `rank train# training validation retained individual` preserves the original
validator ordering; `train#` ranks only finalists. A retained target is not a new discovery.
The optional legacy `Selection source:` line refers to that old selected champion; current
`Leading finalist source:` refers only to the leading finalist, which may not be approved.

### Diagnostic sections

Read available diagnostics in either format with these meanings:

| Section | What to extract and how to interpret it |
|---|---|
| Baseline measurements | Target and named seed training/validation scores, accepted or incompatible status, and aliases after fixed inputs are restored. Seed parameters are evaluated under this round's rules, not the seed strategy's original rules. |
| Baseline comparisons | Per-fold net, trade, forced-closure, cost, drawdown, and breach differences against the target; fitness differences against the target and strongest seed for each metric. Report training and validation disagreements separately. A zero baseline has no percentage improvement. |
| Leader fold diagnostics | Measurements for the leading finalist, distinct shortlist training leader (called `Final training leader` in older reports), distinct best-seen candidate, and approved upgrade when it is a different candidate. The training leader may originate from the archive or protected baselines. Each fold resets state and liquidates remaining positions at its end; these are segment results, not continuous multi-year net. |
| Finalist provenance | First successful search evaluation, including generation 0, plus baseline and catalogue matches. Validation, rescoring, and reporting replays do not establish a discovery generation. A baseline first evaluated during final assembly has `first seen=not observed`. |
| Search observations and workload | Best all-fold fitness ever searched, whether it reached the shortlist, requests, cache computations/reuse, actual simulations, and separate optimisation/reporting durations. Final rescore requests include missing baseline evaluations. Search + rescore requests include cache hits and in-flight waiters; requests are not independent candidates or simulations. |

`Stable progress` uses all-fold fitness and can be compared across generations. Ordinary
`Top Members` scores use rotating folds and cannot. The best-seen candidate may have been
lost from the population and considered through the archive, but archive membership alone
does not guarantee shortlisting or upgrade approval. Its later diagnostic replay cannot change
the decision. Do not add it as an extra champion just because the report exposes it.

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

Use the report filename as provenance. Preserve the status: **upgrade approved**, **legacy
validation-selected; upgrade unassessed**, **diagnostic-only reference**, **completed-finalist
reference with unavailable verdict**, or **unvalidated progress reference**. A base-retained
current run normally adds no new definition. For an approved upgrade, use comments such as:

```scala
  // GA-optimized indicator params for <base> (rules unchanged). Upgrade approved for evaluation by
  // <report-file-name>; objective <mode/version>, policy <version> (recorded research settings).
  // Search score X.XXXXXX; absolute selection score Y.YYYYYY.
  // Selection net improvement <delta> >= <H>; drawdown increase <delta> pp; winning months <n>/<M>.
  // Fixed-trade cost stress: candidate net <value>; improvement <delta> >= <H>.
  // searched <actual period>: <continuous batch metrics from Step 8>
  // historical <actual period>: <continuous batch metrics from Step 8; record prior reuse>
  val <new_name> = TestStrategy(
```

Use `SCGA-optimized` and the actual shuffle status where applicable. Cite the source report
for full configuration and evidence rather than copying its full JSON into code. For legacy
entries, preserve the recorded scores and breaches without calling them approved upgrades.
For incomplete evidence, state what is unavailable. Only include a retention percentage when
the reported objectives are the same and training fitness is positive. If the base was later
deleted, say so in the provenance comment.

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

These continuous runs remain necessary for candidates being added, even after upgrade
approval. They are further evaluation and cannot rewrite the recorded selection decision.
A later batch gain does not turn a rejected challenger into an approved upgrade; a later loss
must be reported and can support retaining the candidate only as a reference within scope.
Record missing or failed measurements as such, never as zero. Format again after updating
comments.

## Step 9 — Report the result

For each source report, state:

- Candidate name and fixed base, GA/SCGA and shuffle status, explicit upgrade decision or
  legacy/reference status, objective and policy versions, search and absolute validation
  scores, and known breaches or incomplete evidence.
- Selection-period net improvement against `H`, drawdown, monthly consistency, cost stress,
  and rejection reason codes for assessed challengers. Explain why a rejected leader differs
  from the approved candidate, or why the base was retained.
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
