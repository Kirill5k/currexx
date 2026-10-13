package currexx.backtest.optimizer

import currexx.algorithms.ValidatedPopulation
import currexx.backtest.TestStrategy

/** The selection decision is complete before reporting and before any forward-period evaluation. */
final case class OptimisationResult(
    finalists: ValidatedPopulation[TestStrategy],
    decision: UpgradeDecision,
    base: SelectionEvidence
)
