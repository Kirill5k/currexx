# Reading optimisation diagnostics

GA and SCGA runs retain their existing Markdown reports in `optimisation-results/`
and console output. Additional diagnostics measure what changed relative to the
round's starting parameters. Diagnostics observe completed selection; they do not
supply candidate state or feed measurements back into search.

## Finalist selection

Each run reserves places for the canonical target and every distinct compatible
effective seed within its configured shortlist size (normally 25). Fixed-input
aliases share one place; incompatible seeds are excluded. A nonpositive shortlist
size or more distinct baselines than places fails before search. Baselines can
occupy the entire shortlist.

A separate bounded search archive retains the strongest distinct candidates by
all-fold fitness, including candidates later lost from the population. Its capacity
equals the shortlist size. Successful search evaluations, including generation 0,
update the archive using existing fold scores, without additional simulations.
The archive is fresh for each optimisation invocation and is never reinserted into
the evolving population. Archive fitness ties use canonical indicator text,
independently of the discovery-generation tie break used by diagnostics.

After evolution, GA fills unreserved places from the archive and final population
by all-fold fitness. SCGA first reserves representatives of species not already
covered by a protected baseline within the representative's configured radius,
where capacity permits, then fills remaining places by all-fold fitness. Forced
assignment to a species at the species cap does not establish that proximity.
Archive membership does not guarantee validation.
Baselines missing an all-fold score are rescored through the existing search cache.
Each shortlisted candidate is validated once after evolution finishes.

The positive-validation gate, validation-first ordering, and 5% tie band remain
unchanged. Before validation, training-fitness ties favour the target, then seeds in
configured order, then discoveries by canonical indicator text. The champion block
identifies whether the target was retained, supplied seed parameters were selected
under the round's rules, or a searched candidate was selected. A seed win can
improve upon the target; it is not automatically a new strategy or a lack of
improvement. Previously promoted seeds may already have been selected on the same
validation period, so these comparisons do not supply independent evidence of
out-of-sample performance.

Scoring formulas, evolution operators, initial seed injection, and random-number
consumption are unchanged. Search archive and finalist assembly failures propagate
as selection failures rather than incomplete diagnostics.

## Fitness and candidate identity

Generation progress uses rotating search folds: one fold is excluded from each
generation's selection score. Compare the separately labelled **all-fold fitness**
across generations. Candidates lost from the final population can remain eligible
through the bounded archive. Equal-score ties can retain a different candidate
from the one identified as best-seen by diagnostics. Reporting a candidate does
not select it or guarantee a shortlist place.

The champion verdict retains the existing validation policy.
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
validation, and report-only backtests cannot create a first appearance. A baseline
first evaluated during final assembly is reported as `not observed` in search.

## Measurement and overhead

After selection finishes, reporting replays only distinct effective candidates
from the target, compatible seeds, leading finalist, shortlist training leader, and
best all-fold candidate seen during search, including one lost from the final
population. Baseline comparisons show fitness and per-fold net differences;
training and validation comparisons remain separate.
The shortlist training leader may come from the final population, archive, or
protected baselines; it need not be the final population's training leader.
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
final rescore requests include any missing baseline evaluations. Computation
attempts count actual cache fills, including retries. Cache reuse
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
  belongs to `IndicatorSearchSpace`, shared by search, finalist assembly, and reporting.
- Keep bounded candidate retention in `SearchArchive` and baseline reservation,
  merging, and species-aware shortlisting in `FinalistAssembler`. Neither depends
  on reporting state. `Validator.shortlisted` owns validation and consensus ordering.
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

## Chronological walk-forward evaluation

Edit the settings at the top of `WalkForwardEvaluator`, then run the object from
the IDE with no program arguments. See [the walk-forward guide](WALK_FORWARD.md)
for the configuration defaults, chronological schedule, reports, and small
real-data verification run.
