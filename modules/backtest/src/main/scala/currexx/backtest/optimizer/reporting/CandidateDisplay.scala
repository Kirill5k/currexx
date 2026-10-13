package currexx.backtest.optimizer.reporting

import currexx.backtest.TestStrategy
import currexx.backtest.optimizer.OptimisationTarget

/** Display the searched component; full candidates remain the keys for selection and caching. */
object CandidateDisplay:
  def forTarget(target: OptimisationTarget): TestStrategy => String = target match
    case OptimisationTarget.Indicator => _.indicator.toString
    case OptimisationTarget.Strategy  => _.rules.toString
    case OptimisationTarget.Both      => _.toString
