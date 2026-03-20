package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import currexx.algorithms.Parameters
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.algorithms.progress.Tracker
import currexx.backtest.optimizer.{IndicatorCrossover, IndicatorEvaluator, IndicatorInitialiser, IndicatorMutator, OptimisationAlgorithm}
import currexx.backtest.optimizer.IndicatorEvaluator.ScoringFunction
import currexx.domain.signal.Indicator

import scala.util.Random

object Optimiser extends IOApp.Simple {

  given Random = Random()

  // Pool size for parallel evaluation
  // Using 3x CPU cores since backtesting is I/O-bound (reading data, processing streams)
  // rather than CPU-bound. Adjust based on available memory.
  val evaluatorPoolSize = Runtime.getRuntime.availableProcessors() * 3

  val gaParameters = Parameters.GA(
    populationSize = 250,
    maxGen = 250,
    crossoverProbability = 0.7,
    mutationProbability = 0.2,
    elitismRatio = 0.025,
    shuffle = true
  )

  val rounds: List[OptimisationRound] = List(
    OptimisationRound(
      name = "s1-balanced",
      strategy = TestStrategy.s1,
      gaParameters = gaParameters,
      // Scoring function selection - see SCORING_GUIDE.md for detailed guide
      // Recommended: Balanced scoring (combines profit, win/loss ratio, and consistency)
      scoringFunction = ScoringFunction.balanced(
        profitWeight = 0.4,      // 40% weight on total profit
        ratioWeight = 0.3,       // 30% weight on win/loss ratio
        consistencyWeight = 0.3, // 30% weight on monthly consistency
        minOrders = Some(50),    // Minimum orders per dataset
        maxOrders = Some(700),   // Maximum orders per dataset
        targetRatio = 2.0        // Target win/loss ratio for normalization
      ),
      testDataSets = MarketDataProvider.majors1h
    )
  )

  // Alternative options:
  // val scoringFunction = ScoringFunction.riskAdjusted(Some(50), Some(700))
  // val scoringFunction = ScoringFunction.totalProfit
  // val scoringFunction = ScoringFunction.medianWinLossRatio(Some(50), Some(700))
  // val scoringFunction = ScoringFunction.averageMedianProfitByMonth

  override def run: IO[Unit] =
    rounds.traverse_ { round =>
      for
        init  <- IndicatorInitialiser.make[IO]
        cross <- IndicatorCrossover.make[IO]
        mut   <- IndicatorMutator.make[IO]
        sel   <- Selector.tournament[IO, Indicator]
        elit  <- Elitism.simple[IO, Indicator]
        eval  <- IndicatorEvaluator.make[IO](
          testFilePaths = round.testDataSets,
          strategy = round.strategy.rules,
          poolSize = evaluatorPoolSize,
          scoringFunction = round.scoringFunction
        )
        markDownProg <- Tracker.markdown[IO, Indicator](
          label = round.name,
          logInterval = 10,
          showTopMember = true,
          showTopN = 3,
          showStats = false,
          finalTopN = 25
        )
        loggingProg <- Tracker.logging[IO, Indicator](
          logInterval = 10,
          showTopMember = true,
          showTopN = 3,
          showStats = false,
          finalTopN = 25
        )
        prog = Tracker.composite(markDownProg, loggingProg)
        _ <- OptimisationAlgorithm
          .ga[IO, Indicator](init, cross, mut, eval, sel, elit, prog)
          .optimise(round.strategy.indicator, round.gaParameters)
      yield ()
    }
}
