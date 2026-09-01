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
    shortlistSize: Int = 25,
    /** Champions of the same indicator shape, mixed into a shuffled round's starting population alongside the strategy's own indicator.
      *
      * The catalogue is a record of what has already scored well under these rules, and a shuffled round throws all of it away. Seeding
      * with the siblings costs nothing and starts the search from several points that are known to work rather than one. Only shapes that
      * can be crossed with the target are usable - `IndicatorInitialiser` drops the rest rather than letting a structural mismatch fail the
      * run mid-flight - and unshuffled rounds ignore these entirely, since their population is the seed by definition.
      */
    extraSeeds: List[Indicator] = Nil
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

  // Shuffled rounds draw three populations and keep the best one's worth of members. Unshuffled rounds read this as 1 whatever it says.
  val gaParametersWithShuffle = gaParameters.copy(shuffle = true, initialOversampling = 3)

  val consistentScoring: ScoringFunction = ScoringFunction.Consistent()

  /** The catalogue as it currently stands, each strategy searched twice — once in file order, once shuffled.
    *
    * Both are kept because shuffling still finds champions the file order misses, but no longer because it finds the best ones, and the
    * case has weakened with every batch. Of the four leading vals by holdout net, s2_optimized and s1_v2_optimized came out of shuffled
    * rounds and s2_optimized_v3 and s5_optimized_v2 out of unshuffled ones. The 2026-08-25/26 batch's one surviving champion came from an
    * unshuffled round whose shuffled twin found nothing at all, and of the six shuffled rounds of 2026-08-31 not one produced a val worth
    * keeping, while four of them breached the closed-trade floor - shuffled populations converge on strategies that trade too rarely to
    * score. On the evidence so far a shuffled round is worth about half of what an unshuffled one is; they are still run because six rounds
    * is a cheap way to keep testing that.
    *
    * Ordered by holdout net, best first. A full pass is long enough that it is routinely interrupted, and this way the strategies most
    * worth improving are the ones already done when it is.
    *
    * s6 is first despite one of the worst holdout nets in the list, because that ordering is a proxy for headroom. It is first as the only
    * val whose parameters came from a hand grid rather than a search, and the round of 2026-08-31 did not change that: its champion ran the
    * trend line to JMA 100, the ceiling introduced the same day, so the search stopped at the bound rather than at an optimum. Another s6
    * round is worth spending once that bound is widened again, and is the first round to spend when it is. Its rules still differ from the
    * s5_optimized_v2 they descend from — a looser reversion leg and no trend exit — so a round on it is not re-deriving s5_optimized_v2's
    * sibling, which is the reason the two vals below are left out.
    *
    * JMA length bounds were widened from [5, 50] to [5, 100] on 2026-08-31, when s6 was added, because s6 uses JMA 90 as its trend line and
    * the old ceiling meant any lineage that mutated that gene collapsed to 50 with no way back - seedable but not searchable. This changes
    * the space every round here searches, not only s6's, since s1_v2_optimized and the s2, s4 and s5 families all search on JMA: a re-run
    * of any of them can now return a slower line than its recorded champion, and the JMA lengths in `ga-optimisation-*.md` reports dated
    * before that change came from a search that could not exceed 50. The first batch run under the wider bound, on 2026-08-31, used it: the
    * s6 champion runs JMA 100 and the top of its shortlist sits between 84 and 100, so the new ceiling binds as the old one did. Elsewhere
    * it barely mattered - the s5_optimized_v2 champion came back with JMA 46, well inside the old range. The mutation step scales with the
    * range, so it is now 9.5 per event rather than 4.5 and walks further.
    *
    * `IndicatorInitialiser` was rebuilt on the same day and the shuffled rounds below are not comparable to the ones that produced the
    * champions recorded in this catalogue. Every bound it draws inside now comes from `GeneBounds`, which the mutator reads too, so a draw
    * can no longer start outside the space mutation is allowed to hold - the old initialiser drew every moving average from [2, 42] against
    * a searchable [5, 100], and NMA's lambda across a range five times wider than the one it could keep. Lengths are drawn log-uniformly
    * rather than uniformly, related lengths as a ratio rather than independently, and the population is now a mixture: 15% copies of the
    * seed, 55% jittered around it at three radii, 30% independent draws. A shuffled round is therefore no longer a purely random start, and
    * the failure it was losing to - candidates whose lines cross on noise, or whose squeeze is smoothed over a shorter window than the ATR
    * it smooths, neither of which trades its way to a score - is the thing those changes are aimed at.
    *
    * Expect breaches rather than clean verdicts. All twelve champions of 2026-08-31 breached at least one constraint and none was rejected
    * for it: the constraints ramp rather than gate, discounting a score instead of zeroing it. The two that breached in every round were
    * concentration (one pair earning a whole month) and the closed-trade floor of 120 on the validation fold, the latter only in shuffled
    * rounds. A champion that breaches is still worth measuring — the two that measured best out of that batch breached two constraints
    * each.
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
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s5_optimized_v2.indicator)
    ),
    OptimisationRound(
      name = "s6_shuffle",
      strategy = TestStrategy.s6,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s5_optimized_v2.indicator)
    ),
    OptimisationRound(
      name = "s2_optimized",
      strategy = TestStrategy.s2_optimized,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s2_optimized_v2.indicator, TestStrategy.s2_optimized_v3.indicator)
    ),
    OptimisationRound(
      name = "s2_optimized_shuffle",
      strategy = TestStrategy.s2_optimized,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s2_optimized_v2.indicator, TestStrategy.s2_optimized_v3.indicator)
    ),
    OptimisationRound(
      name = "s5_optimized_v2",
      strategy = TestStrategy.s5_optimized_v2,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s6.indicator)
    ),
    OptimisationRound(
      name = "s5_optimized_v2_shuffle",
      strategy = TestStrategy.s5_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s6.indicator)
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
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s2_optimized.indicator, TestStrategy.s2_optimized_v3.indicator)
    ),
    OptimisationRound(
      name = "s2_optimized_v2_shuffle",
      strategy = TestStrategy.s2_optimized_v2,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s2_optimized.indicator, TestStrategy.s2_optimized_v3.indicator)
    ),
    OptimisationRound(
      name = "s4_optimized_v1",
      strategy = TestStrategy.s4_optimized_v1,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s4_optimized_v2.indicator)
    ),
    OptimisationRound(
      name = "s4_optimized_v1_shuffle",
      strategy = TestStrategy.s4_optimized_v1,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s4_optimized_v2.indicator)
    ),
    OptimisationRound(
      name = "s12",
      strategy = TestStrategy.s12,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s12_optimized.indicator)
    ),
    OptimisationRound(
      name = "s12_shuffle",
      strategy = TestStrategy.s12,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s12_optimized.indicator)
    ),
    OptimisationRound(
      name = "s12_optimized",
      strategy = TestStrategy.s12_optimized,
      gaParameters = gaParameters,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s12.indicator)
    ),
    OptimisationRound(
      name = "s12_optimized_shuffle",
      strategy = TestStrategy.s12_optimized,
      gaParameters = gaParametersWithShuffle,
      scoringFunction = consistentScoring,
      extraSeeds = List(TestStrategy.s12.indicator)
    )
  )

  override def run: IO[Unit] =
    rounds.traverse_ { round =>
      for
        init  <- IndicatorInitialiser.seeded[IO](round.extraSeeds)
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
