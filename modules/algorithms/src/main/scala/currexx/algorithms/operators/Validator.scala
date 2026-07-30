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

  /** Re-scores the best `shortlistSize` distinct individuals of a finished population and reorders it by what they scored.
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
  def shortlisted[F[_]: Applicative, I](shortlistSize: Int)(objectiveFn: I => F[Fitness]): F[Validator[F, I]] =
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
          // Sorted by validation fitness, so that the head of the population a run hands back is the candidate that did
          // best where the search had no reach rather than the one that fitted the sample hardest. `sortBy` is stable
          // and the input arrives in training order, so candidates tied on validation — which is every candidate of a
          // run that found nothing — keep their training ranking rather than an arbitrary one.
          .map(_.sortBy(_._3)(using Ordering[Fitness].reverse))
    }.pure[F]

  /** Repeats each individual's fitness as its own validation fitness, for searches with no held-out evidence to offer.
    *
    * Honest about what it does and not about what it means: the second figure is the first one again, so anything reading it as an
    * out-of-sample result is reading a number the search maximised. Here so that a run without a holdout still type-checks, not as a default
    * worth having.
    */
  def none[F[_]: Applicative, I]: F[Validator[F, I]] =
    new Validator[F, I] {
      override def validate(population: EvaluatedPopulation[I]): F[ValidatedPopulation[I]] =
        population.map { case (individual, fitness) => (individual, fitness, fitness) }.pure[F]
    }.pure[F]
