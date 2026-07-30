package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import currexx.algorithms.{EvaluatedPopulation, Parameters}
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.algorithms.progress.Tracker
import currexx.backtest.MarketDataProvider.Dataset
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
    trainingDataSets: List[Dataset],
    validationDataSets: List[Dataset],
    /** How many of the finished population are replayed against `validationDataSets` and ranked on it.
      *
      * Small on purpose, and the reason the whole arrangement works. A run scores on the order of 10^5 candidates against its training
      * data, and the best of 10^5 noisy readings of one sample is a large number whatever the scoring function is — which is why the winner
      * of a search is not evidence. Ranking a shortlist of this size on data the search never saw is the same trick played at a scale where
      * it costs almost nothing: the luckiest of 25 is barely luckier than the median of 25.
      *
      * Raising it is not free. Every extra finalist is another draw against the validation set, and the set is only as good as the number
      * of times it has been consulted; take it far enough and the validation data has been searched over too, just more slowly. It belongs
      * to the round rather than to the optimiser because that budget is spent against a particular validation set — two rounds sharing one
      * set are consulting it between them, and only a per-round figure makes that visible where it is decided.
      */
    shortlistSize: Int = 25
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
          trainingData = round.trainingDataSets,
          strategy = round.strategy.rules,
          poolSize = evaluatorPoolSize,
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
          .ga[IO, Indicator](init, cross, mut, eval.evaluator, sel, elit, prog)
          .optimise(round.strategy.indicator, round.gaParameters)
        _ <- selectChampion(round, eval.validate, prog, finalPop)
      yield ()
    }

  /** One finalist, read against both halves of the data. */
  final private case class Candidate(
      indicator: Indicator,
      trainingRank: Int,
      trainingScore: Double,
      validationScore: Double,
      breaches: List[ScoringFunction.Violation]
  ) {

    /** What fraction of its training score survived the move to unseen data. The number the whole run is really about: a search that found
      * something reports a figure near 1.0, and one that fitted its sample reports a figure near 0.
      */
    def retained: Option[Double] = Option.when(trainingScore > 0.0)(validationScore / trainingScore)
  }

  /** Picks a round's champion by replaying its finalists against data the search never scored against, and records how each of them fared
    * on both halves.
    *
    * Finishing first is not evidence of anything. Selection ranks on training data, so the top of a finished population is the candidate
    * that fitted the sample best, and a fitness function cannot separate a real edge from a well-fitted one when it is computed on the very
    * data being maximised over — the discounts and constraints get fitted along with everything else. The only reading that carries
    * information is one taken where the search had no reach, which is what this does.
    *
    * The whole shortlist is recorded rather than just the winner, because the distribution is the diagnosis. Finalists that hold most of
    * their training score mean the search found something and the remaining question is which; finalists that collapse to zero mean it
    * found nothing, however good the training numbers look, and no amount of picking between them will change that.
    *
    * The verdict goes to the tracker rather than to stdout, so that it is recorded wherever the round's results are, and lasts as long as
    * they do. It is the shortlist in that same file that a strategy is eventually picked from, and this is what says which entry of it can
    * be trusted, if any.
    */
  private def selectChampion(
      round: OptimisationRound,
      validate: Indicator => IO[List[OrderStats]],
      tracker: Tracker[IO, Indicator],
      population: EvaluatedPopulation[Indicator]
  ): IO[Unit] = {
    val title = s"Champion selection: ${round.name}"
    // Deduplicated first, because a converged population is mostly copies of a handful of individuals, and a shortlist
    // of one candidate repeated twenty-five times is no shortlist at all.
    val finalists = population.distinctBy(_._1).take(round.shortlistSize).toList.zipWithIndex

    if (finalists.isEmpty) tracker.displayNote(title, List("No candidates were evaluated."))
    else
      finalists
        .traverse { case ((indicator, fitness), idx) =>
          validate(indicator).map { stats =>
            Candidate(
              indicator = indicator,
              trainingRank = idx + 1,
              trainingScore = fitness.value,
              validationScore = round.scoringFunction.score(stats),
              breaches = round.scoringFunction.violations(stats)
            )
          }
        }
        .flatMap(candidates => tracker.displayNote(title, verdict(round, candidates)))
  }

  private def verdict(round: OptimisationRound, candidates: List[Candidate]): List[String] = {
    // Ties broken by training rank so that the ordering is total and reproducible, which matters because a round that
    // found nothing has every candidate tied on a validation score of zero.
    val ranked  = candidates.sortBy(candidate => (-candidate.validationScore, candidate.trainingRank))
    val chosen  = ranked.head
    val zeroes  = candidates.count(_.validationScore <= 0.0)
    val percent = (value: Double) => f"${value * 100}%.1f%%"

    val preamble = List(
      s"Trained on ${round.trainingDataSets.size} dataset(s): ${round.trainingDataSets.mkString(", ")}",
      s"Validated on ${round.validationDataSets.size} dataset(s): ${round.validationDataSets.mkString(", ")}",
      "",
      s"${candidates.size} distinct finalist(s), $zeroes of which scored zero on validation data.",
      ""
    )

    val table =
      "  rank  train#    training  validation  retained  breaches" ::
        ranked.zipWithIndex.map { case (candidate, idx) =>
          f"  ${idx + 1}%4d  ${candidate.trainingRank}%6d  ${candidate.trainingScore}%10.6f  ${candidate.validationScore}%10.6f  " +
            f"${candidate.retained.fold("n/a")(percent)}%8s  ${candidate.breaches.size}%8d"
        }

    val trainingWinner = ranked.indexWhere(_.trainingRank == 1) + 1
    val displacement   =
      if (chosen.trainingRank == 1) Nil
      else List("", s"The training-ranked #1 placed $trainingWinner of ${candidates.size} on validation and was not selected.")

    val outcome =
      if (chosen.validationScore <= 0.0)
        List(
          "",
          "NOTHING SELECTED: no finalist scored above zero on data it was never searched against.",
          "Whatever the training figures say, this round did not find an edge that exists outside its own sample.",
          s"Best by validation, recorded so the round leaves a trace and not as a candidate: ${chosen.indicator}"
        )
      else {
        val summary =
          f"SELECTED (best of ${candidates.size} on validation): training ${chosen.trainingScore}%.6f -> " +
            f"validation ${chosen.validationScore}%.6f, retaining ${chosen.retained.fold("n/a")(percent)}%s"
        val breachLines =
          if (chosen.breaches.isEmpty) List("Satisfies every constraint on validation data.")
          else s"BREACHES ${chosen.breaches.size} constraint(s) on validation data:" :: chosen.breaches.map(breach => s"  - $breach")
        "" :: summary :: breachLines ::: List(s"Indicator: ${chosen.indicator}")
      }

    preamble ::: table ::: displacement ::: outcome
  }
}
