package currexx.backtest

import currexx.algorithms.Parameters
import currexx.backtest.optimizer.IndicatorEvaluator.ScoringFunction

final case class OptimisationRound(
    name: String,
    strategy: TestStrategy,
    gaParameters: Parameters.GA,
    scoringFunction: ScoringFunction,
    testDataSets: List[String]
)
