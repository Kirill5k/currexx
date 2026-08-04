package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import currexx.algorithms.{Parameters, ValidatedPopulation}
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.algorithms.progress.Tracker
import currexx.backtest.MarketDataProvider.Dataset
import currexx.backtest.optimizer.{
  IndicatorCrossover,
  IndicatorInitialiser,
  IndicatorMutator,
  IndicatorObjective,
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
    trainingDataSets: List[Dataset],
    validationDataSets: List[Dataset],
    shortlistSize: Int = 25
)

object Optimiser extends IOApp.Simple {

  given Random = Random()

  val evaluatorPoolSize = Runtime.getRuntime.availableProcessors()

  val gaParameters = Parameters.GA(
    populationSize = 300,
    maxGen = 75,
    crossoverProbability = 0.7,
    mutationProbability = 0.1,
    elitismRatio = 0.02,
    shuffle = false
  )

  val gaParametersWithShuffle = gaParameters.copy(shuffle = true)

  val consistentScoring: ScoringFunction = ScoringFunction.Consistent()

  val rounds: List[OptimisationRound] = List(
    OptimisationRound(
      name = "s1_optimized",
      strategy = TestStrategy.s1_optimized,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s1_optimized_shuffle",
      strategy = TestStrategy.s1_optimized,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s1_v2",
      strategy = TestStrategy.s1_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s1_v2_shuffle",
      strategy = TestStrategy.s1_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s1_v2_optimized_v2",
      strategy = TestStrategy.s1_v2_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s1_v2_optimized_v2_shuffle",
      strategy = TestStrategy.s1_v2_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s2",
      strategy = TestStrategy.s2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s2_shuffle",
      strategy = TestStrategy.s2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s2_optimized_v2",
      strategy = TestStrategy.s2_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s2_optimized_v2_shuffle",
      strategy = TestStrategy.s2_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s4_optimized",
      strategy = TestStrategy.s4_optimized,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s4_optimized_shuffle",
      strategy = TestStrategy.s4_optimized,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s4_regime_optimized_v2",
      strategy = TestStrategy.s4_regime_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s4_regime_optimized_v2_shuffle",
      strategy = TestStrategy.s4_regime_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s5_optimized_v2",
      strategy = TestStrategy.s5_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s5_optimized_v2_shuffle",
      strategy = TestStrategy.s5_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s5_optimized_v2",
      strategy = TestStrategy.s5_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s5_optimized_v2_shuffle",
      strategy = TestStrategy.s5_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s12",
      strategy = TestStrategy.s12,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s12_shuffle",
      strategy = TestStrategy.s12,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s12_optimized",
      strategy = TestStrategy.s12_optimized,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
    ),
    OptimisationRound(
      name = "s12_optimized_shuffle",
      strategy = TestStrategy.s12_optimized,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      trainingDataSets = MarketDataProvider.majors1hTraining,
      validationDataSets = MarketDataProvider.majors1hValidation
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
        obj   <- IndicatorObjective.make[IO](
          trainingData = round.trainingDataSets,
          strategy = round.strategy.rules,
          poolSize = evaluatorPoolSize,
          shortlistSize = round.shortlistSize,
          validationData = round.validationDataSets,
          scoringFunction = round.scoringFunction
        )
        markDownProg <- Tracker.markdown[IO, Indicator](
          label = round.name,
          logInterval = 10,
          showTopMember = true,
          showTopN = 3,
          showStats = false,
          finalTopN = round.shortlistSize
        )
        loggingProg <- Tracker.logging[IO, Indicator](
          logInterval = 10,
          showTopMember = true,
          showTopN = 3,
          showStats = false,
          finalTopN = round.shortlistSize
        )
        prog = Tracker.composite(markDownProg, loggingProg)
        finalPop <- OptimisationAlgorithm
          .ga[IO, Indicator](init, cross, mut, obj.evaluator, obj.validator, sel, elit, prog)
          .optimise(round.strategy.indicator, round.gaParameters)
        _ <- reportChampion(round, obj.validate, prog, finalPop)
      yield ()
    }

  /** Records how a round's finalists fared on both halves of the data, and says whether the one at the top of them can be trusted.
    *
    * The population arrives already validated and already ranked on it, because `Op.ValidatePopulation` is the last step of the search
    * itself — so the champion is `population.head` and nothing here chooses anything. What is left is the reading: the whole shortlist
    * rather than only the winner, because the distribution is the diagnosis. Finalists that hold most of their training score mean the
    * search found something and the remaining question is which; finalists that collapse to zero mean it found nothing, however good the
    * training figures look, and no amount of picking between them will change that.
    *
    * The verdict goes to the tracker rather than to stdout, so that it is recorded wherever the round's results are and lasts as long as
    * they do. It is the shortlist in that same file that a strategy is eventually picked from, and this is what says which entry of it can
    * be trusted, if any.
    */
  private def reportChampion(
      round: OptimisationRound,
      validate: Indicator => IO[List[OrderStats]],
      tracker: Tracker[IO, Indicator],
      population: ValidatedPopulation[Indicator]
  ): IO[Unit] = {
    val title = s"Champion selection: ${round.name}"
    population.headOption match
      case None                   => tracker.displayNote(title, List("No candidates were evaluated."))
      case Some((champion, _, _)) =>
        validate(champion)
          .map(round.scoringFunction.violations)
          .flatMap(breaches => tracker.displayNote(title, verdict(round, population, breaches)))
  }

  /** What the tracker's own final report cannot know: which corpus this round was given, and whether the candidate at the top of it is fit
    * to use.
    *
    * Everything derivable from the population itself — the table, the count that scored zero, whether validating changed the answer — is
    * rendered by the tracker, because it is true of any validated run and not of this one in particular. What is left here is the two
    * things only the round holds: the datasets, which the population has no memory of, and the verdict on the champion, which needs the
    * scoring function that produced it.
    */
  private def verdict(
      round: OptimisationRound,
      population: ValidatedPopulation[Indicator],
      championBreaches: List[ScoringFunction.Violation]
  ): List[String] = {
    val (champion, championTraining, championValidation) = population.head
    val retained                                         =
      if (championTraining.value > 0.0) f"${championValidation.value / championTraining.value * 100}%.1f%%" else "n/a"

    val datasets = List(
      s"Trained on ${round.trainingDataSets.size} dataset(s): ${round.trainingDataSets.mkString(", ")}",
      s"Validated on ${round.validationDataSets.size} dataset(s): ${round.validationDataSets.mkString(", ")}",
      ""
    )

    val outcome =
      if (championValidation.value <= 0.0)
        List(
          "NOTHING SELECTED: no finalist scored above zero on data it was never searched against.",
          "Whatever the training figures say, this round did not find an edge that exists outside its own sample.",
          s"Best by validation, recorded so the round leaves a trace and not as a candidate: $champion"
        )
      else {
        val summary =
          f"SELECTED (best of ${population.size} on validation): training ${championTraining.value}%.6f -> " +
            f"validation ${championValidation.value}%.6f, retaining $retained%s"
        val breachLines =
          if (championBreaches.isEmpty) List("Satisfies every constraint on validation data.")
          else s"BREACHES ${championBreaches.size} constraint(s) on validation data:" :: championBreaches.map(breach => s"  - $breach")
        summary :: breachLines ::: List(s"Indicator: $champion")
      }

    datasets ::: outcome
  }
}
