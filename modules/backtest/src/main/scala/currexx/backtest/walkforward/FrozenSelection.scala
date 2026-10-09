package currexx.backtest.walkforward

import currexx.backtest.TestStrategy
import currexx.backtest.optimizer.{OptimisationResult, SelectionEvidence, UpgradeDecision}

enum SelectionOutcome:
  case UpgradeApproved, BaseRetained

/** The complete strategy is fixed before the forward evaluator can see its test period. */
final case class FrozenSelection(
    strategy: TestStrategy,
    trainingFitness: Option[Double],
    selectionFitness: Option[Double],
    decision: UpgradeDecision,
    baseEvidence: SelectionEvidence
):
  def outcome: SelectionOutcome = decision match
    case _: UpgradeDecision.Approved   => SelectionOutcome.UpgradeApproved
    case _: UpgradeDecision.RetainBase => SelectionOutcome.BaseRetained

object FrozenSelection:
  /** Converts an already completed decision. Forward evaluation cannot apply another acceptance rule. */
  def fromResult(base: TestStrategy, result: OptimisationResult): FrozenSelection =
    val indicator = result.decision match
      case UpgradeDecision.Approved(candidate, _, _) => candidate
      case UpgradeDecision.RetainBase(_)             => base.indicator
    val fitness = result.finalists.find(_._1 == indicator)
    FrozenSelection(
      base.copy(indicator = indicator),
      fitness.map(_._2.value),
      fitness.map(_._3.value),
      result.decision,
      result.base
    )
