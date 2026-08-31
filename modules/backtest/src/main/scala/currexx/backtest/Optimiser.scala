package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import currexx.algorithms.{Parameters, ValidatedPopulation}
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.algorithms.progress.Tracker
import currexx.backtest.MarketDataProvider.Corpus
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
    corpus: Corpus = MarketDataProvider.majors1hCorpus,
    shortlistSize: Int = 25
)

object Optimiser extends IOApp.Simple {

  given Random = Random()

  val evaluatorPoolSize = Runtime.getRuntime.availableProcessors()

  val gaParameters = Parameters.GA(
    populationSize = 300,
    maxGen = 100,
    crossoverProbability = 0.7,
    mutationProbability = 0.1,
    elitismRatio = 0.02,
    shuffle = false
  )

  val gaParametersWithShuffle = gaParameters.copy(shuffle = true)

  val consistentScoring: ScoringFunction = ScoringFunction.Consistent()

  /** The catalogue as it currently stands, each strategy searched twice — once in file order, once shuffled.
    *
    * Both are kept because shuffling still finds champions the file order misses, but no longer because it finds the best ones: of the four
    * leading vals by holdout net, s2_optimized and s1_v2_optimized came out of shuffled rounds and s2_optimized_v3 and s5_optimized_v2 out
    * of unshuffled ones. The 2026-08-25/26 batch made the case weaker still: its one surviving champion, s5_optimized_v2, came out of an
    * unshuffled round whose shuffled twin found nothing at all.
    *
    * Ordered by holdout net, best first. A full pass is long enough that it is routinely interrupted, and this way the strategies most
    * worth improving are the ones already done when it is.
    *
    * s6 is first despite the worst holdout net but one, because that ordering is a proxy for headroom and on s6 the proxy is wrong: every
    * other val here is already a GA champion, and s6 is the only one whose parameters were set by a hand grid over the two searched years.
    * It leads the catalogue on in-sample net and has never been searched, so it is the entry with the most left to find. Its rules also
    * differ from the s5_optimized_v2 they descend from — a looser reversion leg and no trend exit — so a round on it is not re-deriving
    * s5_optimized_v2's sibling, which is the reason the two vals below are left out.
    *
    * READ BEFORE SPENDING A RUN ON s6: the search cannot reach its trend length. s6 uses JMA 90 and `IndicatorMutator` clamps JMA length to
    * [5, 50], while `IndicatorInitialiser` draws it from [2, 42]. So the unshuffled round starts every member at JMA 90 and collapses it to
    * 50 the first time that gene mutates, with no way back, and the shuffled round never sees 90 at all. Nothing above 50 is explorable
    * either way. That parameter is worth roughly 1000 of s6's in-sample net against a length of 50, so the champion of an s6 round is
    * likely to be a worse strategy than s6 with a better fitness — measured against a corpus, not against s6. Two smaller cases of the same
    * thing: the squeeze smoothing (SMA 50) and the trend JMA power (1) are outside what the initialiser draws, though mutation can reach
    * both.
    *
    * Widening the JMA bound would fix it, and would also change the search space of every other round in this list, since s1_v2_optimized
    * and the s2, s4 and s5 families all search on JMA. That is a decision about the whole catalogue rather than about s6, so it is left
    * unmade here.
    *
    * Also expect a trade-count breach: s6 closed 116 trades on the validation fold against a floor of 120. That constraint ramps rather
    * than gates, so it discounts the score instead of zeroing it, but a champion trading even slightly less often than s6 will breach it
    * too.
    *
    * Two vals in `BatchBacktester` are not searched here: s2_optimized_v3 and s4_optimized_v2. Each is a GA descendant of a base that is
    * searched, so a round on one would largely re-derive its own sibling. Add them if that stops being true — s2_optimized_v3 currently
    * leads the catalogue on holdout net, so it is the more defensible of the two to promote into a round of its own.
    */
  val rounds: List[OptimisationRound] = List(
    OptimisationRound(
      name = "s6",
      strategy = TestStrategy.s6,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s6_shuffle",
      strategy = TestStrategy.s6,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s2_optimized",
      strategy = TestStrategy.s2_optimized,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s2_optimized_shuffle",
      strategy = TestStrategy.s2_optimized,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s5_optimized_v2",
      strategy = TestStrategy.s5_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s5_optimized_v2_shuffle",
      strategy = TestStrategy.s5_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s1_v2_optimized",
      strategy = TestStrategy.s1_v2_optimized,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s1_v2_optimized_shuffle",
      strategy = TestStrategy.s1_v2_optimized,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s2_optimized_v2",
      strategy = TestStrategy.s2_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s2_optimized_v2_shuffle",
      strategy = TestStrategy.s2_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s4_optimized_v1",
      strategy = TestStrategy.s4_optimized_v1,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s4_optimized_v1_shuffle",
      strategy = TestStrategy.s4_optimized_v1,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s12",
      strategy = TestStrategy.s12,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s12_shuffle",
      strategy = TestStrategy.s12,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s12_optimized",
      strategy = TestStrategy.s12_optimized,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring
    ),
    OptimisationRound(
      name = "s12_optimized_shuffle",
      strategy = TestStrategy.s12_optimized,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring
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
          corpus = round.corpus,
          strategy = round.strategy.rules,
          poolSize = evaluatorPoolSize,
          shortlistSize = round.shortlistSize,
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
          label = round.name,
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
