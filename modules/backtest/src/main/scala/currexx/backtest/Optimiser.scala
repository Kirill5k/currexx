package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import currexx.algorithms.{Parameters, ValidatedPopulation}
import currexx.algorithms.operators.Validator
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.optimizer.{OptimisationAlgorithm, ScoringFunction}
import currexx.domain.signal.Indicator

import scala.util.Random

final case class OptimisationRound(
    name: String,
    strategy: TestStrategy,
    parameters: Parameters.GA | Parameters.SCGA,
    scoringFunction: ScoringFunction,
    corpus: Corpus = MarketDataProvider.majors1hCorpus,
    shortlistSize: Int = 25,
    /** Champions of the same indicator shape, mixed into the starting population alongside the strategy's own indicator.
      *
      * The catalogue is a record of what has already scored well under these rules, and starting from several points known to work costs
      * nothing over starting from one. Both mixes use them: a shuffled round leans on them heavily, having thrown everything else away, and
      * an unshuffled one keeps enough of them to have something worth crossing its seed with. Only shapes that can be crossed with the
      * target are usable. The round's search space filters incompatible schemas and restores fixed values before mixing seeds.
      */
    extraSeeds: List[Indicator] = Nil,
    /** Additional indicators or composite subtrees to keep at their target values. All identical occurrences are fixed; values absent from
      * the strategy produce a validation error. Raw-close identity inputs and value trackers unused by these rules are fixed automatically.
      */
    fixedIndicators: Set[Indicator] = Set.empty
)

object Optimiser extends IOApp.Simple {

  given Random = Random()

  val evaluatorPoolSize = Runtime.getRuntime.availableProcessors()

  val gaParameters = Parameters.GA(
    populationSize = 300,
    maxGen = 150,
    crossoverProbability = 0.7,
    mutationProbability = 0.1,
    elitismRatio = 0.02,
    shuffle = false
  )

  // Three populations drawn and the best one's worth of members kept. Worth it here and not on an unshuffled round, whose members mostly
  // are the seed: over-drawing selects on variation, and there has to be some to select on.
  // Opt a round into SCGA with parameters = Parameters.SCGA.fromGA(gaParameters), or convert the shuffled settings below.
  // Radius 0.15, eight species and 10% interspecies mating are provisional defaults, not values tuned on historical results.
  val gaParametersWithShuffle = gaParameters.copy(shuffle = true, initialOversampling = 3)

  val consistentScoring: ScoringFunction = ScoringFunction.Consistent()

  /** Strategy families searched with both refining and exploring starting populations. `shuffle` changes the population mix, not the order
    * of market data. Each round evaluates its own strategy's rules; extra seeds contribute indicator parameters only.
    *
    * s10_v2, s6, s13 and s1_v2 have different entry or exit rules from the existing families, so they need their own rounds. s13 keeps CMF
    * as its momentum value tracker and RSX as its exit-zone detector; the price-momentum families have incompatible seed schemas. s10_v2
    * reused the later evaluation period during manual development; its historical results are not independent validation. All rounds retain
    * the standard search/validation corpus.
    */
  val rounds: List[OptimisationRound] = List(
    OptimisationRound(
      name = "s2_optimized",
      strategy = TestStrategy.s2_optimized,
      parameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s2_optimized_v2.indicator)
    ),
    OptimisationRound(
      name = "s2_optimized_shuffle",
      strategy = TestStrategy.s2_optimized,
      parameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s2_optimized_v2.indicator)
    ),
    OptimisationRound(
      name = "s10",
      strategy = TestStrategy.s10,
      parameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s10_shuffle",
      strategy = TestStrategy.s10,
      parameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s10_v2",
      strategy = TestStrategy.s10_v2,
      parameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s10_v2_shuffle",
      strategy = TestStrategy.s10_v2,
      parameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s5_optimized_v2",
      strategy = TestStrategy.s5_optimized_v2,
      parameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(
        TestStrategy.s5_optimized_v3.indicator,
        TestStrategy.s6.indicator,
        TestStrategy.s6_optimized.indicator
      )
    ),
    OptimisationRound(
      name = "s5_optimized_v2_shuffle",
      strategy = TestStrategy.s5_optimized_v2,
      parameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(
        TestStrategy.s5_optimized_v3.indicator,
        TestStrategy.s6.indicator,
        TestStrategy.s6_optimized.indicator
      )
    ),
    OptimisationRound(
      name = "s5_optimized_v2_scga",
      strategy = TestStrategy.s5_optimized_v2,
      parameters = Parameters.SCGA.from(gaParametersWithShuffle),
      scoringFunction = consistentScoring,
      extraSeeds = List(
        TestStrategy.s5_optimized_v3.indicator,
        TestStrategy.s6.indicator,
        TestStrategy.s6_optimized.indicator
      )
    ),
    OptimisationRound(
      name = "s4_optimized_v2",
      strategy = TestStrategy.s4_optimized_v2,
      parameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s4_optimized_v1.indicator, TestStrategy.s4_optimized_v1.indicator)
    ),
    OptimisationRound(
      name = "s4_optimized_v2_shuffle",
      strategy = TestStrategy.s4_optimized_v2,
      parameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s4_optimized_v1.indicator, TestStrategy.s4_optimized_v1.indicator)
    ),
    OptimisationRound(
      name = "s6_optimized",
      strategy = TestStrategy.s6_optimized,
      parameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s6.indicator)
    ),
    OptimisationRound(
      name = "s6_optimized_shuffle",
      strategy = TestStrategy.s6_optimized,
      parameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s6.indicator)
    ),
    OptimisationRound(
      name = "s13",
      strategy = TestStrategy.s13,
      parameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s13_shuffle",
      strategy = TestStrategy.s13,
      parameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s1_v2_optimized",
      strategy = TestStrategy.s1_v2_optimized,
      parameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s1_v2_optimized_shuffle",
      strategy = TestStrategy.s1_v2_optimized,
      parameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    )
  )

  override def run: IO[Unit] =
    rounds.traverse_ { round =>
      for
        algorithm <- OptimisationAlgorithm.indicator[IO](round, evaluatorPoolSize)
        finalPop  <- algorithm.optimise
        title = s"Champion selection: ${round.name}"
        _ <- finalPop.headOption match
          case None =>
            algorithm.tracker.displayNote(title, List("No candidates were evaluated."))
          case Some((champion, _, _)) =>
            algorithm
              .validate(champion)
              .map(round.scoringFunction.violations)
              .flatMap(breaches => algorithm.tracker.displayNote(title, verdict(round, finalPop, breaches)))
      yield ()
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

    val datasets = round.corpus.describe :+ ""

    val outcome =
      if (championValidation.value <= 0.0)
        List(
          "NOTHING SELECTED: no finalist scored above zero on data it was never searched against.",
          "Whatever the training figures say, this round did not find an edge that exists outside its own sample.",
          s"Best by validation, recorded so the round leaves a trace and not as a candidate: $champion"
        )
      else {
        val summary =
          f"SELECTED (from ${population.size} after validation, ties inside ${Validator.defaultTieBand.describe}%s broken on training): " +
            f"training ${championTraining.value}%.6f -> validation ${championValidation.value}%.6f, retaining $retained%s"
        val breachLines =
          if (championBreaches.isEmpty) List("Satisfies every constraint on validation data.")
          else s"BREACHES ${championBreaches.size} constraint(s) on validation data:" :: championBreaches.map(breach => s"  - $breach")
        summary :: breachLines ::: List(s"Indicator: $champion")
      }

    datasets ::: outcome
  }
}
