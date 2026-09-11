package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import currexx.algorithms.{Parameters, ValidatedPopulation}
import currexx.algorithms.operators.{Elitism, Selector, Validator}
import currexx.algorithms.progress.Tracker
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.optimizer.{
  IndicatorObjective,
  IndicatorSearchOperators,
  IndicatorSearchSpace,
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
  val gaParametersWithShuffle = gaParameters.copy(shuffle = true, initialOversampling = 3)

  val consistentScoring: ScoringFunction = ScoringFunction.Consistent()

  /** The holdout leaders the catalogue still spends GA budget on, each searched twice — file order, then shuffled.
    *
    * A round earns its place by being the best of its family on the holdout and by having somewhere left to go. That rules out a val a
    * descendant already beats, because searching from the weaker seed mostly rediscovers the stronger one; a family whose problem is its
    * rules rather than its parameters, which no amount of indicator search will fix; and a val whose parameters came from a hand grid that
    * the GA has already failed to improve on. Everything dropped for one of those reasons stays in `TestStrategy` and can come back as an
    * `extraSeeds` entry, which costs nothing.
    *
    * Both twins run because the shuffled one keeps earning it: it surfaces training-fitness leaders that validation ranking misses, and
    * both September promotions (`s2_optimized_v4`, `s5_optimized_v3`) arrived that way with a validation figure of 0.000000.
    *
    * Ordered by holdout net, best first, because a full pass is long enough to be interrupted routinely and this way the rounds most worth
    * having are the ones already done when it is. s10_v2 is an explicitly requested research seed: its later evaluation period was reused
    * during manual development, so its historical net is not independent validation. Its rounds retain the standard search/validation
    * corpus.
    */
  val rounds: List[OptimisationRound] = List(
    OptimisationRound(
      name = "s2_optimized_v4",
      strategy = TestStrategy.s2_optimized_v4,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s2_optimized_v3.indicator, TestStrategy.s2_optimized.indicator)
    ),
    OptimisationRound(
      name = "s2_optimized_v4_shuffle",
      strategy = TestStrategy.s2_optimized_v4,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s2_optimized_v3.indicator, TestStrategy.s2_optimized.indicator)
    ),
    OptimisationRound(
      name = "s10",
      strategy = TestStrategy.s10,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s10_shuffle",
      strategy = TestStrategy.s10,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s5_optimized_v2",
      strategy = TestStrategy.s5_optimized_v2,
      gaParameters = gaParameters,
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
      gaParameters = gaParametersWithShuffle,
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
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s4_optimized_v1.indicator, TestStrategy.s4_optimized_v3.indicator)
    ),
    OptimisationRound(
      name = "s4_optimized_v2_shuffle",
      strategy = TestStrategy.s4_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s4_optimized_v1.indicator, TestStrategy.s4_optimized_v3.indicator)
    )
  )

  override def run: IO[Unit] =
    rounds.traverse_ { round =>
      for
        space  <- IO.fromEither(IndicatorSearchSpace.forStrategy(round.strategy, round.fixedIndicators))
        search <- IndicatorSearchOperators.make[IO](space, round.extraSeeds)
        sel    <- Selector.tournament[IO, Indicator]
        elit   <- Elitism.simple[IO, Indicator]
        obj    <- IndicatorObjective.make[IO](
          corpus = round.corpus,
          strategy = round.strategy.rules,
          poolSize = evaluatorPoolSize,
          shortlistSize = round.shortlistSize,
          scoringFunction = round.scoringFunction,
          searchSpace = Some(space)
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
          label = round.name,
          logInterval = 10,
          showTopMember = true,
          showTopN = 3,
          showStats = false,
          finalTopN = round.shortlistSize
        )
        prog = Tracker.composite(markDownProg, loggingProg)
        finalPop <- OptimisationAlgorithm
          .ga[IO, Indicator](search.initialiser, search.crossover, search.mutator, obj.evaluator, obj.validator, sel, elit, prog)
          .optimise(round.strategy.indicator, round.gaParameters)
        _ <- prog.displayNote(
          "Search space",
          List(
            "Indicator structure, sources and roles are fixed; only searchable numeric parameters can evolve.",
            "Raw-close identity inputs and value trackers unused by the rules are pinned to the target, alongside explicit fixed subtrees."
          ) ++ (if (space.fixedIndicators.isEmpty) List("No fixed indicators.")
                else space.fixedIndicators.toList.map(indicator => s"Fixed: $indicator").sorted)
        )
        _ <- reportChampion(round, obj.validate, prog, finalPop)
      yield ()
    }

  /** Records how a round's finalists fared on both halves of the data, and says whether the one at the top of them can be trusted.
    *
    * The population arrives already validated and already ranked, because `Op.ValidatePopulation` is the last step of the search itself —
    * so the champion is `population.head` and nothing here chooses anything. That ranking leads on the held-out score and falls back to the
    * training rank only among candidates the held-out score could not separate, by the band `Validator.defaultTieBand` sets. What is left
    * is the reading: the whole shortlist rather than only the winner, because the distribution is the diagnosis. Finalists that hold most
    * of their training score mean the search found something and the remaining question is which; finalists that collapse to zero mean it
    * found nothing, however good the training figures look, and no amount of picking between them will change that.
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
