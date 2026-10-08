# Walk-forward evaluation

Open `currexx.backtest.WalkForwardEvaluator` and edit the settings at the top of
the object, then run it directly from the IDE. No program arguments are needed.

The defaults are `roundName = "s13_ga_refine"`, `masterSeed = 42L`,
`history = MarketDataProvider.majors1hHistory`, `plan = WalkForwardPlan.default`,
and `evaluatorPoolSize = Runtime.getRuntime.availableProcessors()`. Choose another
named preset from `OptimisationRounds.rounds` and set the seed explicitly before
the run.
Each preset keeps its existing GA/SCGA parameters, scoring, extra seeds, fixed
inputs and shortlist policy. A full run executes five independent searches and
can be expensive.

The same configured object can also be run through sbt:

```sh
sbt "backtest/runMain currexx.backtest.WalkForwardEvaluator"
```

The default schedule starts with 12 months of training, followed by four months of
selection and four months of testing. It advances four months at a time, expanding
training from July 2023. The five test periods cover November 2024–June 2026.

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
`yyyyMMdd-HHmmss-SSS` format with the round name, without a UUID. An existing
experiment directory is preserved and causes the new run to fail.

The directory contains:

- `manifest.json`: full settings, original strategy, dates, per-window seeds and
  SHA-256 fingerprints of the original CSV files.
- `preflight.json`: per-period bar availability, warm-up loss and executable bars,
  written before any search.
- `window-N-frozen.json`: the complete selection decision, written before testing.
- `window-N-result.json`: paired financial metrics and execution coverage.
- `summary.json` and `report.md`: completed-window summaries and experiment status.
- `window-N-failure.json`: a failed stage, if the experiment stops.

Window seeds use the recorded `walk-forward-v2` derivation: explicit month boundaries,
including exclusive end months. Seeds no longer depend on the display format of a
date range. They differ from the earlier `walk-forward-v1` derivation for the same
master seed.

Existing optimiser reports remain in `optimisation-results/`, labelled with the
experiment and window. Failed experiments retain completed artifacts and stop;
automatic resume is not supported.

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

This runs two windows on one real currency pair with a two-member population and
one generation. Its reports are under `target/walk-forward-smoke/`. It verifies the
pipeline and persistence; its small search budget is not a strategy-quality test.
