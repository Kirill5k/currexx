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

  // Pool size for parallel evaluation.
  // Backtesting replays in-memory market data through pure indicator calculations — CPU-bound,
  // not I/O-bound. Optimal pool size equals available CPU cores to avoid context-switching overhead.
  val evaluatorPoolSize = Runtime.getRuntime.availableProcessors()

  val gaParameters = Parameters.GA(
    populationSize = 250,
    maxGen = 350,
    crossoverProbability = 0.7,
    mutationProbability = 0.2,
    elitismRatio = 0.025,
    shuffle = false
  )

  val gaParametersWithShuffle = gaParameters.copy(shuffle = true)

  val robustScoring: ScoringFunction = ScoringFunction.robust()

  // Rounds seed from surviving strategies only (deprecated/low-performing seeds were pruned from
  // TestStrategy). Re-seed additional rounds from any current strategy's indicator as needed.
  val rounds: List[OptimisationRound] = List(
    OptimisationRound(
      name = "s1_optimized",
      strategy = TestStrategy.s1_optimized,
      gaParameters = gaParameters,
      scoringFunction = robustScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s1_optimized_shuffle",
      strategy = TestStrategy.s1_optimized,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = robustScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s1_v2",
      strategy = TestStrategy.s1_v2,
      gaParameters = gaParameters,
      scoringFunction = robustScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s1_v2_shuffle",
      strategy = TestStrategy.s1_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = robustScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s1_v2_optimized_v2",
      strategy = TestStrategy.s1_v2_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = robustScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s1_v2_optimized_v2_shuffle",
      strategy = TestStrategy.s1_v2_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = robustScoring,
      testDataSets = MarketDataProvider.majors1h
    )
  )

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
