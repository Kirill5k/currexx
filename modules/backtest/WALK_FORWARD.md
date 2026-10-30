# Walk-forward evaluation

Open `currexx.backtest.WalkForwardEvaluator` and edit the settings at the top of
the object, then run it directly from the IDE. No program arguments are needed.

Each window uses the selected round's optimisation target. Set that target to
`Indicator`, `Strategy` or `Both` in `OptimisationRounds`. The default is `Both`.
The complete selected indicators and rules are frozen before the forward test.
See [Configure optimisation](OPTIMISATION.md) for rule bounds, defaults, input
checks, seed compatibility and scheduled rounds. Each window uses these same
rules. The manifest records the configured bounds and complete base strategy.

The default configuration compares two search objectives separately for five fixed base strategies:

| Setting | Value |
|---|---|
| Rounds (`roundNames`) | `s2_optimized_ga_refine`, `s12_optimized_ga_refine`, `s13_ga_refine`, `s1_v2_optimized_ga_refine`, `s6_optimized_ga_refine` |
| Target | `Both` |
| Search objectives | `Current`, `BaselineRelative(weight = 0.5)` |
| Master seeds | `42L`, `43L` |
| History | `MarketDataProvider.majors1hHistory`: six major currency pairs, hourly bars |
| Schedule | `WalkForwardPlan.default`: five expanding windows |
| Evaluator pool | `Runtime.getRuntime.availableProcessors()` |

This is **20 experiments and 100 window searches**: five rounds × two objectives × two
master seeds × five windows. All round names are resolved before searches begin.
Rounds and their runs execute sequentially; each search uses the configured
evaluator pool. A full matrix can be expensive.

Within each round, only the search objective and master seed vary. The refinement preset
keeps a population of 300, 150 generations, crossover probability 0.7, mutation
probability 0.1, elitism ratio 0.02, `shuffle = false`, and shortlist size 25.
`ScoringFunction.Consistent()`, fixed inputs, execution costs, capital, and the
`trial-v1` upgrade policy are shared across these experiments. Each round uses its
own base strategy and complete named seeds from `OptimisationRounds`:

| Base | Named seeds |
|---|---|
| `s2_optimized` | `s2_optimized_v2` |
| `s12_optimized` | `s12` |
| `s13` | `s13_optimized` |
| `s1_v2_optimized` | `s1_v2_optimized_v2` |
| `s6_optimized` | `s6`, `s5_optimized_v2`, `s5_optimized_v3` |

The target selects their active components before compatibility and duplicate
checks. The baseline report records which seeds were accepted or excluded and why.

`BaselineRelative(0.5)` adjusts the training quality score according to
net improvement over the same base on the same fold. A zero quality score stays
zero. Both objectives still use the same absolute selection fitness and upgrade
requirements; changing the search objective does not relax approval.

Run the [small verification run](#small-verification-run) first; it checks the full
history coverage before exercising a reduced search matrix. To launch the full
100-search matrix, run the configured object through sbt:

```sh
sbt "backtest/runMain currexx.backtest.WalkForwardEvaluator"
```

The default schedule starts with 12 months of training, followed by four months of
selection and four months of testing. It advances four months at a time, expanding
training from July 2023. The five test periods cover November 2024–June 2026.
Ranges in this table include both displayed months:

| Window | Training | Selection | Forward test |
|---|---|---|---|
| 1 | Jul 2023–Jun 2024 | Jul–Oct 2024 | Nov 2024–Feb 2025 |
| 2 | Jul 2023–Oct 2024 | Nov 2024–Feb 2025 | Mar–Jun 2025 |
| 3 | Jul 2023–Feb 2025 | Mar–Jun 2025 | Jul–Oct 2025 |
| 4 | Jul 2023–Jun 2025 | Jul–Oct 2025 | Nov 2025–Feb 2026 |
| 5 | Jul 2023–Oct 2025 | Nov 2025–Feb 2026 | Mar–Jun 2026 |

Each search receives only its training and selection ranges. Its first passing
finalist is frozen before testing. If no finalist passes, the unchanged original
base is retained. Every forward test compares that frozen choice with the same
original base; later searches do not inherit earlier winners or react to test
results. Test periods may become selection/training history in later rounds.

Each period starts flat and closes remaining positions at its end. CSV boundaries
within a period preserve positions and indicator history. Prior prices provide
lookback only; the first in-period window primes the simulator and executions
begin on the next bar. The cost model and capital are identical for both strategies.

Before any search, a metadata scan checks every requested month and sufficient bars
for warm-up and execution across the complete schedule. Missing history fails the
experiment immediately, identifying the affected window. The oldest training fold
has no earlier prices: its first 99 trading bars form the initial price window in
every expanding round. The report highlights this loss rather than hiding it in
forward coverage. Every period also skips one complete window to prime fresh state.

## Results

`WalkForwardReportStore` owns the output location, defaulting to
`outputDirectory = Path("walk-forward-results")`. Experiments write to
`walk-forward-results/<experiment-id>/`. Each ID combines a UTC timestamp in
`yyyyMMdd-HHmmss-SSS` format with the round name, objective and master seed,
without a UUID. An existing experiment directory is preserved and causes the
new run to fail.

The directory contains:

- `manifest.json`: search target, configured rule bounds, complete seeds, full settings,
  original strategy, dates, per-window seeds and
  SHA-256 fingerprints of the original CSV files.
- `preflight.json`: per-period bar availability, warm-up loss and executable bars,
  written before any search.
- `search/`: per-window optimisation Markdown reports, labelled with the
  experiment and window.
- `window-N-frozen.json`: the complete selection decision, written before testing.
- `window-N-result.json`: paired financial metrics and execution coverage.
- `summary.json` and `report.md`: completed-window summaries and experiment status.
- `window-N-failure.json`: a failed stage, if the experiment stops.

Reports use format version `3`. Candidate evidence and decisions store the
complete `TestStrategy`.

Window seeds use the recorded `walk-forward-v2` derivation: explicit month
boundaries, including exclusive end months. The derivation is independent of
the display format of a date range.

Standalone `Optimiser` runs continue to write reports to `optimisation-results/`.
Walk-forward search reports stay inside each experiment's `search/` directory.
Failed experiments retain completed artifacts and stop; automatic resume is not
supported.

After each round's four experiments complete, a sibling `<first-experiment-id>-comparison/`
directory contains `comparison.json` and `report.md`. Each round gets its own
comparison before the next round starts. The comparison reports each
objective/seed run's total, median and worst forward net difference from the fixed
base, positive windows, and approved upgrades.

Compare the two objectives **for the same round, master seed and window**. Their derived
window seed, base, date ranges and execution settings then match. Start with the
10 paired forward-net differences between objectives per round, and check whether the
direction survives across seeds and windows; do not select an objective from its
best individual run or compare the objectives' differently scaled search scores.
Use each experiment's per-window results for drawdown, costs and trade activity.

If every selection retains the base, every paired forward net difference should
be zero. That means neither search produced an upgrade under the frozen policy.
Inspect rejection codes and measured/required values in `window-N-frozen.json`
and the linked optimisation reports to distinguish unprofitable candidates from
failures of consistency, concentration, drawdown or cost constraints. Keep the
policy fixed for this comparison rather than loosening it after seeing failures.

Results are **retrospective development evidence**, not independent confirmation:
this market history has already influenced the strategy catalogue. The report
summarises paired net differences across separate periods. It does not manufacture
a continuous account's drawdown or Sharpe ratio, choose scoring weights from the
forward results, or promote strategy definitions.

## Small verification run

Run `currexx.backtest.walkforward.WalkForwardSmoke` from the IDE in the backtest
module's test sources. It needs no program arguments. The optional sbt equivalent
is:

```sh
sbt "backtest/Test/runMain currexx.backtest.walkforward.WalkForwardSmoke"
```

The smoke entry point reads the rounds, objectives, master seeds, history and plan
from `WalkForwardEvaluator`, so it verifies the configured experiments rather than
separate strategy presets. It performs two stages:

1. **Full data preflight:** inspect all six configured pairs and all five windows
   without running a search. Success prints
   `Full history preflight passed: 6 pairs, 5 windows`. Missing data stops the run.
2. **Reduced search matrix:** run all five rounds, both objectives and both master seeds on
   the first two windows and first currency pair. This is 20 small experiments
   and 40 searches, each with population 6, one generation, elitism 0.5 and
   shortlist size 6. The target, configured complete strategy seeds, scoring, costs
   and upgrade policy remain configured as above. Seed compatibility and
   deduplication still apply; each initial population must fit the base and all
   distinct accepted seeds.

Each experiment must complete both paired tests. A retained base must reproduce
exactly zero forward net difference. The smoke writes experiment artifacts,
including each experiment's `search/` reports, and a separate objective comparison per round under
`target/walk-forward-smoke/`; the full-history preflight is reported in the console,
while each smoke experiment's `preflight.json` describes its reduced two-window,
one-pair schedule.

There is no separate preflight-only command. Running this smoke does **not**
launch the full 100-search matrix or change its budget. It verifies data coverage,
objective/seed wiring and persistence; its small search budget is not a
strategy-quality test.
