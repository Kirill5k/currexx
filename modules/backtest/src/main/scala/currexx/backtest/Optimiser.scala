package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import currexx.algorithms.{EvaluatedPopulation, Parameters}
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.algorithms.progress.Tracker
import currexx.backtest.optimizer.{
  IndicatorCrossover,
  IndicatorEvaluator,
  IndicatorInitialiser,
  IndicatorMutator,
  OptimisationAlgorithm,
  ScoringFunction
}
import currexx.domain.signal.Indicator

import scala.util.Random

final case class OptimisationRound(
    name: String,
    strategy: TestStrategy,
    gaParameters: Parameters.GA,
    scoringFunction: ScoringFunction,
    testDataSets: List[String]
)

object Optimiser extends IOApp.Simple {

  given Random = Random()

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

  val consistentScoring: ScoringFunction = ScoringFunction.Consistent()

  val rounds: List[OptimisationRound] = List(
    OptimisationRound(
      name = "s2",
      strategy = TestStrategy.s2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s2_shuffle",
      strategy = TestStrategy.s2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s2_optimized_v2",
      strategy = TestStrategy.s2_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s2_optimized_v2_shuffle",
      strategy = TestStrategy.s2_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s4_optimized",
      strategy = TestStrategy.s4_optimized,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s4_optimized_shuffle",
      strategy = TestStrategy.s4_optimized,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s4_regime_optimized_v2",
      strategy = TestStrategy.s4_regime_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s4_regime_optimized_v2_shuffle",
      strategy = TestStrategy.s4_regime_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
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
        _ <- reportChampion(round, eval.backtest, prog, finalPop)
      yield ()
    }

  /** Reports whether the best candidate a round produced actually satisfies the constraints it was scored against.
    *
    * The scoring function discounts a breach instead of rejecting it, so that selection has a gradient to climb, which means finishing
    * first is no longer evidence of being acceptable. Nothing else downstream asks the question, and the population is sorted by a score
    * that has already blended every constraint into one number, so the breach is invisible by the time a result is written out.
    *
    * The verdict goes to the tracker rather than to stdout, so that it is recorded wherever the round's results are, and lasts as long as
    * they do. It is the shortlist in that same file that a strategy is eventually picked from, and this is what says whether the candidate
    * at the top of it can be trusted.
    */
  private def reportChampion(
      round: OptimisationRound,
      backtest: Indicator => IO[List[OrderStats]],
      tracker: Tracker[IO, Indicator],
      population: EvaluatedPopulation[Indicator]
  ): IO[Unit] =
    val title = s"Champion: ${round.name}"
    population.headOption match
      case None                      => tracker.displayNote(title, List("No candidates were evaluated."))
      case Some((champion, fitness)) =>
        backtest(champion).flatMap { stats =>
          val breaches = round.scoringFunction.violations(stats)
          val verdict  =
            if (breaches.isEmpty) List("Satisfies every constraint.")
            else s"BREACHES ${breaches.size} constraint(s) despite winning:" :: breaches.map(breach => s"  - $breach")
          tracker.displayNote(title, f"Fitness: ${fitness.value}%.6f" :: verdict ::: List(s"Indicator: $champion"))
        }
}
