package currexx.backtest.walkforward

import currexx.algorithms.ValidatedPopulation
import currexx.algorithms.operators.Validator
import currexx.backtest.TestStrategy
import currexx.domain.signal.Indicator

enum SelectionOutcome:
  case CandidateSelected, BaseSelected, NoCandidatePassed

/** The complete strategy is fixed before the forward evaluator can see its test period. */
final case class FrozenSelection(
    strategy: TestStrategy,
    outcome: SelectionOutcome,
    trainingFitness: Option[Double],
    selectionFitness: Option[Double]
)

object FrozenSelection:
  def choose(base: TestStrategy, finalists: ValidatedPopulation[Indicator]): FrozenSelection =
    finalists.find(candidate => Validator.passesGate(candidate._3)) match
      case Some((indicator, training, selection)) =>
        val strategy = base.copy(indicator = indicator)
        val outcome  = if (strategy == base) SelectionOutcome.BaseSelected else SelectionOutcome.CandidateSelected
        FrozenSelection(strategy, outcome, Some(training.value), Some(selection.value))
      case None => FrozenSelection(base, SelectionOutcome.NoCandidatePassed, None, None)
