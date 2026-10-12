package currexx.backtest.optimizer.indicator

import currexx.algorithms.ValidatedPopulation
import currexx.domain.signal.Indicator

/** The selection decision is complete before reporting and before any forward-period evaluation. */
final case class OptimisationResult(
    finalists: ValidatedPopulation[Indicator],
    decision: UpgradeDecision,
    base: SelectionEvidence
)
