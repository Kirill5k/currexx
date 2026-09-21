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
    /** Compatible parameter sets mixed into the starting population alongside the target, always evaluated under the target's rules. Both
      * refinement and exploration divide their clone/jitter allocation among these seeds. The search space filters incompatible schemas,
      * restores fixed values and removes duplicate projections before mixing them.
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

  // Exploration draws three populations and retains the best population's worth of members.
  // SCGA.from preserves these settings; its species defaults remain unchanged for the comparison.
  val gaParametersWithShuffle            = gaParameters.copy(shuffle = true, initialOversampling = 3)
  val consistentScoring: ScoringFunction = ScoringFunction.Consistent()

  final private case class Family(
      name: String,
      strategy: TestStrategy,
      extraSeeds: List[Indicator] = Nil,
      // Include one additional SCGA exploration round.
      runScga: Boolean = false
  ) {
    def toRound(params: Parameters.GA | Parameters.SCGA, mode: String): OptimisationRound =
      OptimisationRound(
        name = s"${name}_${params.name.toLowerCase}_$mode",
        strategy = strategy,
        parameters = params,
        scoringFunction = consistentScoring,
        extraSeeds = extraSeeds
      )
  }

  private val families = List(
    Family(
      "s2_optimized",
      TestStrategy.s2_optimized,
      List(TestStrategy.s2_optimized_v2.indicator)
    ),
    Family(
      "s10_optimized",
      TestStrategy.s10_optimized,
      List(TestStrategy.s10.indicator)
    ),
    Family(
      "s5_optimized_v2",
      TestStrategy.s5_optimized_v2,
      List(TestStrategy.s5_optimized_v3.indicator, TestStrategy.s6.indicator, TestStrategy.s6_optimized.indicator)
    ),
    Family(
      "s4_optimized_v2",
      TestStrategy.s4_optimized_v2,
      List(TestStrategy.s4_optimized_v1.indicator)
    ),
    Family(
      "s6_optimized",
      TestStrategy.s6_optimized,
      List(TestStrategy.s6.indicator, TestStrategy.s5_optimized_v2.indicator, TestStrategy.s5_optimized_v3.indicator)
    ),
    Family("s10_v2", TestStrategy.s10_v2, runScga = true),
    Family("s13", TestStrategy.s13, runScga = true),
    Family("s1_v2_optimized", TestStrategy.s1_v2_optimized, runScga = true)
  )

  /** Every family receives refining and exploring GA rounds, plus an exploring SCGA round when enabled. `shuffle` changes the initial
    * population mix, not market-data order. Compatible extra seeds contribute parameters only; each round retains its own rules and fixed
    * inputs. All rounds use the standard search/validation corpus, excluding the historical period reused during development.
    */
  val rounds: List[OptimisationRound] = families.flatMap { family =>
    List(
      family.toRound(gaParameters, "refine"),
      family.toRound(gaParametersWithShuffle, "explore")
    ) ::: Option.when(family.runScga)(family.toRound(Parameters.SCGA.from(gaParametersWithShuffle), "explore")).toList
  }

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
