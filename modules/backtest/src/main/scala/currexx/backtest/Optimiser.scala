package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import currexx.algorithms.{EvaluatedPopulation, Parameters}
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
    mutationProbability = 0.08,
    elitismRatio = 0.025,
    shuffle = false
  )

  val gaParametersWithShuffle = gaParameters.copy(shuffle = true)

  // One config drives both jobs: the scoring function ramps towards these thresholds during the search, and the
  // champion is re-checked against the same numbers afterwards.
  val robustConfig                   = ScoringFunction.RobustConfig()
  val robustScoring: ScoringFunction = ScoringFunction.robust(robustConfig)

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
        finalPop <- OptimisationAlgorithm
          .ga[IO, Indicator](init, cross, mut, eval.evaluator, sel, elit, prog)
          .optimise(round.strategy.indicator, round.gaParameters)
        _ <- reportChampion(round, eval.backtest, finalPop)
      yield ()
    }

  /** Reports whether the best candidate a round produced actually satisfies the constraints it was scored against.
    *
    * The scoring function discounts a breach instead of rejecting it, so that selection has a gradient to climb, which means finishing
    * first is no longer evidence of being acceptable. Nothing else downstream asks the question, and the population is sorted by a score
    * that has already blended every constraint into one number, so the breach is invisible by the time a result is written out.
    */
  private def reportChampion(
      round: OptimisationRound,
      backtest: Indicator => IO[List[OrderStats]],
      population: EvaluatedPopulation[Indicator]
  ): IO[Unit] =
    population.headOption match
      case None                      => IO.println(s"[${round.name}] no candidates were evaluated")
      case Some((champion, fitness)) =>
        for
          stats <- backtest(champion)
          breaches = ScoringFunction.violations(stats, robustConfig)
          _ <- IO.println(f"[${round.name}] champion fitness ${fitness.value}%.6f")
          _ <-
            if (breaches.isEmpty) IO.println(s"[${round.name}] satisfies every constraint")
            else
              IO.println(s"[${round.name}] BREACHES ${breaches.size} constraint(s) despite winning:") *>
                breaches.traverse_(breach => IO.println(s"[${round.name}]   - $breach"))
          _ <- IO.println(s"[${round.name}] champion: $champion")
        yield ()
}
