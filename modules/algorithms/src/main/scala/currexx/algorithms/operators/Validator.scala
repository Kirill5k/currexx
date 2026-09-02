package currexx.algorithms.operators

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.{EvaluatedPopulation, Fitness, ValidatedPopulation}

/** Re-scores a finished population against evidence the search was never allowed to read, so that a champion is chosen on something other
  * than how well it fitted the sample every candidate was fitted to.
  *
  * The search maximises fitness, which makes fitness a good signal to climb and a poor one to conclude from: the best of tens of thousands
  * of noisy readings of one sample is a large number whether or not anything was found. `Evaluator` answers "which of these is climbing
  * fastest"; this answers "does any of it survive contact with evidence the climb could not see", and the two questions need different data
  * or the second is not being asked at all.
  *
  * Runs once, on the finished population, rather than per generation — a held-out sample is only held out for as long as nothing has
  * optimised against it, and every consultation spends a little of that.
  */
trait Validator[F[_], I]:
  def validate(population: EvaluatedPopulation[I]): F[ValidatedPopulation[I]]

object Validator:

  /** Re-scores the best `shortlistSize` distinct individuals of a finished population and reorders it by what they scored — see
    * `consensusOrder` for what "reorders" means, which is not simply the best validation figure first.
    *
    * Only a shortlist, and that is what makes the arrangement work rather than a corner cut for speed. A search scores on the order of 10^5
    * candidates against its training data, and the best of 10^5 noisy readings of one sample is a large number whether or not anything was
    * found — which is why a search's own winner is not evidence. Re-scoring a handful on evidence the search never saw is the same trick
    * played at a scale where it costs almost nothing: the luckiest of twenty-five is barely luckier than the median of twenty-five. Raising
    * the figure is not free, because held-out evidence is only held out for as long as little has been chosen against it.
    *
    * @param shortlistSize
    *   how many distinct individuals to spend on the held-out evidence.
    * @param objectiveFn
    *   scores one individual against that evidence. Nothing here can check that it reads different data from the evaluator's; a caller that
    *   passes the training objective gets a population that has been scored twice and validated not at all.
    */
  def shortlisted[F[_]: Applicative, I](shortlistSize: Int, tieBand: TieBand = defaultTieBand)(
      objectiveFn: I => F[Fitness]
  ): F[Validator[F, I]] =
    new Validator[F, I] {
      override def validate(population: EvaluatedPopulation[I]): F[ValidatedPopulation[I]] =
        population
          // Deduplicated before truncating, because a converged population is mostly copies of a handful of
          // individuals and a shortlist of one candidate repeated twenty-five times is no shortlist at all.
          .distinctBy(_._1)
          .take(shortlistSize)
          .traverse { case (individual, trainingFitness) =>
            objectiveFn(individual).map(validationFitness => (individual, trainingFitness, validationFitness))
          }
          .map(consensusOrder(_, tieBand))
    }.pure[F]

  /** How close to the best held-out score a candidate has to come before the training rank is allowed to separate them, and how to say so.
    *
    * Passed in rather than hardcoded so that the one empirical number in this file can be varied by a test, and so that a caller reporting
    * what it selected under reads the figure rather than repeating it in prose that can drift away from it.
    *
    * @param relative
    *   the fraction of the best score inside which two candidates count as tied. Set from the spread actually observed: across the eleven
    *   rounds of 2026-09-01/02 the gap between the best and second-best non-zero validation scores was either tiny or large with almost
    *   nothing in between - 0.1%, 3.5% and 4.0% in three rounds, and 17.9% or more in the other seven - so five percent sits in that gap
    *   and fires only on differences indistinguishable from noise. Do not tune it against the holdout; see `defaultTieBand`.
    */
  final case class TieBand(relative: Double):
    def describe: String                               = f"${relative * 100}%.0f%%"
    def ties(best: Double, candidate: Double): Boolean = candidate >= best * (1.0 - relative)

  /** The band every round currently runs under.
    *
    * Deliberately not swept against the holdout corpus, however tempting: the holdout's only value is that nothing has been chosen against
    * it, and tuning a selection rule on it makes it development data like any other. Fixing this number needs a period held back from that
    * too, or a nested split inside the search folds.
    */
  val defaultTieBand: TieBand = TieBand(0.05)

  /** The finished shortlist in the order a champion should be taken from, which is not the order of the validation score alone.
    *
    * Held-out evidence gates and then leads: a candidate that earned nothing out of sample is out regardless of what it did in sample, and
    * a candidate that clearly beat the field out of sample wins. That is unchanged. What changed on 2026-09-02 is what happens when the top
    * of the field is separated by nothing.
    *
    * Over the eleven rounds of 2026-09-01/02, 186 of 275 finalists scored exactly zero out of sample, so a shortlist of twenty-five was
    * really a choice between about eight — and those eight are not near-copies whose ordering hardly matters, since the median finalist
    * differs from the top one in nine to twelve of its twelve or thirteen genes. In one round the top two came in at 0.5714 and 0.5708. A
    * plain argmax reads a difference of a tenth of a percent between two substantially different strategies as a decision.
    *
    * So candidates within `tieBand` of the best held-out score are treated as tied on it, and the tie is broken by the training rank. That
    * is not a re-admission of the fitting the validation step exists to filter: every candidate in the band has already cleared the gate
    * and matched the best out-of-sample result, so the training figure is only choosing among strategies the held-out evidence could not
    * separate. Outside the band nothing changes, which is the property a rank-sum over both scores would have lost — ranks discard
    * magnitude, and replaying one over these rounds traded a validation of 0.1048 for 0.0045 to gain 0.02 of training.
    *
    * Candidates that failed the gate keep their training order behind the rest, so a run that found nothing still hands back its
    * training-ranked best and reports itself as having selected nothing.
    */
  private def consensusOrder[I](validated: ValidatedPopulation[I], tieBand: TieBand): ValidatedPopulation[I] =
    val (survived, failed) = validated.partition(_._3.value > 0.0)
    if (survived.isEmpty) failed
    else
      val best               = survived.map(_._3.value).max
      val (contenders, rest) = survived.partition(c => tieBand.ties(best, c._3.value))
      // Stable throughout, and the input arrives in training order, so anything these leave tied keeps its training ranking.
      val led    = contenders.sortBy(c => (-c._2.value, -c._3.value))
      val others = rest.sortBy(_._3)(using Ordering[Fitness].reverse)
      led ++ others ++ failed

  /** Repeats each individual's fitness as its own validation fitness, for searches with no held-out evidence to offer.
    *
    * Honest about what it does and not about what it means: the second figure is the first one again, so anything reading it as an
    * out-of-sample result is reading a number the search maximised. Here so that a run without a holdout still type-checks, not as a
    * default worth having.
    */
  def none[F[_]: Applicative, I]: F[Validator[F, I]] =
    new Validator[F, I] {
      override def validate(population: EvaluatedPopulation[I]): F[ValidatedPopulation[I]] =
        population.map { case (individual, fitness) => (individual, fitness, fitness) }.pure[F]
    }.pure[F]
