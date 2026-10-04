# Reading optimisation diagnostics

GA and SCGA runs retain their existing Markdown reports in `optimisation-results/`
and console output. Additional diagnostics measure what changed relative to the
round's starting parameters. They do not alter scoring, selection, the search
budget, or random-number generation.

## Fitness and candidate identity

Generation progress uses rotating search folds: one fold is excluded from each
generation's selection score. Compare the separately labelled **all-fold fitness**
across generations. Its best-seen candidate may have been lost from the final
population; reporting it does not reinsert it into the shortlist.

The final shortlist and champion verdict retain the existing selection policy.
When every finalist fails validation, the first finalist is recorded for
diagnostics only. A higher training score does not establish better validation
performance, and neither establishes fresh out-of-sample performance.

Named seeds contribute indicator parameters, evaluated under the **target's
rules**. The baseline section records every supplied seed, including incompatible
ones and aliases that become identical after fixed inputs are restored. Aliases
share measurements. Catalogue duplicate checks additionally require equal rules;
matching parameters under different rules are listed separately as parameter-only
matches, not strategy duplicates.

First appearance means the first successful search evaluation of a canonical
candidate. Generation 0 includes oversampled initial candidates. Rescoring,
validation, and report-only backtests cannot create a first appearance.

## Measurement and overhead

After selection finishes, reporting replays only distinct effective candidates
from the target, compatible seeds, leading finalist, final training leader, and
best all-fold candidate seen during search, including one lost from the final
population. Baseline comparisons show fitness and per-fold net differences;
training and validation comparisons remain separate.
Each fold is reduced immediately to fitness, net profit, trade and forced-closure
counts, costs, drawdown, and constraint breaches. Full trade and equity histories
are not retained in the search cache.

Fold measurements keep each fold's resets and terminal liquidations. They are
not a replacement for a continuous multi-year portfolio backtest. Missing
validation measurements and percentage changes from a zero baseline are shown
as unavailable, not zero.

Workload is split into search plus rescoring, validation, reporting, and direct
backtests. Both validation for selection and explicit validation replays belong
to validation; raw `backtest` calls belong to direct backtests. Only diagnostic
inspection belongs to reporting. Evaluation requests include reused candidates;
computation attempts count actual cache fills, including retries. Cache reuse
includes callers waiting on an in-flight
computation. Fold and pair simulation counts are measured at execution, rather
than estimated from population size. Started and completed counts can differ
after a failure.

Search counters are frozen before diagnostic replays. Diagnostic duration and
workload are reported separately. If diagnostics fail, the already-written final
results remain available, an incomplete-diagnostics note is attempted, and the
completed finalists are returned so later rounds can continue. Missing metrics
are never replaced with fabricated measurements. Search and selection failures
still propagate; recovery applies only to reporting after selection finishes.

## Maintaining the implementation

- Register strategy names once in `StrategyCatalogue`; its batch flag controls
  whether `BatchBacktester` evaluates an entry. Keep lineage entries registered
  for duplicate detection. A completeness test checks the catalogue against every
  public `TestStrategy` definition, including newly added ones.
- Supply extra seeds as `NamedIndicator(name, indicator)` values. Seed resolution
  belongs to `IndicatorSearchSpace`, shared by search and reporting.
- Keep observation and counters in `RunDiagnostics`, pooled simulation execution
  in `IndicatorBacktest`, evaluation and diagnostic inspection in
  `IndicatorObjective`, report assembly in `OptimisationReportBuilder`, and text
  formatting, including failure-note wording, in `OptimisationReportRenderer`.
  `ReportingTracker` writes completed reports and explicit failure notes through
  the existing output interface; it neither builds reports nor recovers failures.
  `OptimisationAlgorithm` owns recovery after selection, so diagnostic failures
  leave completed finalists available and let subsequent rounds continue.
- Reporting must never consume randomness, consult historical holdout data, or
  feed measurements back into selection. Seeded replay configuration and JSON
  export are outside this feature.
