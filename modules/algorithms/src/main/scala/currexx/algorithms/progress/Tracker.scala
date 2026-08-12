package currexx.algorithms.progress

import cats.Monad
import cats.effect.Async
import currexx.algorithms.{EvaluatedPopulation, Fitness, Parameters, ValidatedPopulation}

import java.time.Instant

trait Tracker[F[_], I]:
  def displayInitial(target: I, params: Parameters.GA): F[Unit]
  def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit]

  /** Reports the finished run, which is the one place a population arrives carrying both of its fitnesses. Progress is reported on training
    * fitness alone because that is all a generation has; a result is not, because training fitness alone is what a run says about itself.
    */
  def displayFinal(population: ValidatedPopulation[I]): F[Unit]
  def displayNote(title: String, lines: List[String]): F[Unit]

  protected def progressMsg(currentGen: Int, maxGen: Int): String =
    s"Generation $currentGen out of $maxGen"

  private def memberMsg(idx: Int, individual: I, fitness: Fitness): String =
    s"#${idx + 1}: $fitness - $individual"

  protected def membersMsg(population: EvaluatedPopulation[I], topN: Int): String =
    population
      .take(topN)
      .zipWithIndex
      .map { case ((individual, fitness), idx) => s"  ${memberMsg(idx, individual, fitness)}" }
      .mkString("\n")

  /** Where the search itself ranked each candidate, recovered by re-sorting on training fitness.
    *
    * A validated population arrives ordered by validation fitness, and it was taken off one ordered by training fitness, so sorting it back
    * reproduces the ranking the search produced. That ordering is the thing worth contrasting the validated one against — it is what the
    * run would have reported about itself.
    */
  private def trainingOrder(population: ValidatedPopulation[I]): Vector[I] =
    population.sortBy(_._2)(using Ordering[Fitness].reverse).map(_._1)

  /** The fraction of a candidate's training score that survived the move to unseen evidence. The number a finished run is really reporting:
    * a search that found something lands near 1.0, one that fitted its sample lands near 0.
    */
  private def retainedMsg(training: Fitness, validation: Fitness): String =
    if (training.value > 0.0) f"${validation.value / training.value * 100}%.1f%%" else "n/a"

  /** The finished population as a table of both its fitnesses, in the order the validator left it.
    *
    * Fixed-width rather than one line per member, because the columns only mean anything read against each other: a validation figure alone
    * says nothing without the training figure it fell from, and neither says anything without `train#` to show whether validating changed
    * the answer.
    */
  protected def validatedMembersMsg(population: ValidatedPopulation[I], topN: Int): String =
    val ranks = trainingOrder(population)
    ("  rank  train#    training  validation  retained  individual" ::
      population.take(topN).toList.zipWithIndex.map { case ((individual, training, validation), idx) =>
        f"  ${idx + 1}%4d  ${ranks.indexOf(individual) + 1}%6d  ${training.value}%10.6f  ${validation.value}%10.6f  " +
          f"${retainedMsg(training, validation)}%8s  $individual"
      }).mkString("\n")

  /** What the validated population says about the run as a whole, before anything is said about any one candidate.
    *
    * The count of candidates that scored zero is the diagnosis rather than a detail: finalists holding most of their training score mean
    * the search found something and the remaining question is which, and finalists collapsing to zero mean it found nothing however good
    * the training figures look. The displacement line says whether validating changed the answer at all, which is the cheapest evidence
    * that the holdout is doing any work.
    */
  protected def validationSummaryMsg(population: ValidatedPopulation[I]): String =
    if (population.isEmpty) "No candidates were validated."
    else
      val zeroes  = population.count(_._3.value <= 0.0)
      val placed  = population.indexWhere(_._1 == trainingOrder(population).head) + 1
      val summary = s"${population.size} finalist(s) validated, $zeroes of which scored zero on evidence the search never saw."
      if (placed == 1) summary
      else s"$summary\nThe training-ranked #1 placed $placed of ${population.size} on validation."

  protected def statsMsg(population: EvaluatedPopulation[I]): String =
    val fitnesses = population.map(_._2.value)
    val avg       = fitnesses.sum / fitnesses.size
    val best      = population.head._2.value
    val worst     = population.last._2.value
    s"Stats: Best=$best, Avg=$avg, Worst=$worst"

  /** Reported on validation fitness, since that is what the population is ordered by and what the run is to be judged on. Guarded against
    * an empty population, which `displayFinal` can be handed by a run that produced nothing and where a reporter throwing would lose the
    * record of that.
    */
  protected def validatedStatsMsg(population: ValidatedPopulation[I]): String =
    if (population.isEmpty) "Stats: no validated candidates"
    else
      val fitnesses = population.map(_._3.value)
      val avg       = fitnesses.sum / fitnesses.size
      val best      = population.head._3.value
      val worst     = population.last._3.value
      s"Stats: Best=$best, Avg=$avg, Worst=$worst"

  protected def durationMsg(start: Instant, end: Instant): String =
    val totalMs = end.toEpochMilli - start.toEpochMilli
    val hours   = totalMs / 3600000
    val minutes = (totalMs % 3600000) / 60000
    val seconds = (totalMs % 60000) / 1000
    val ms      = totalMs  % 1000
    val parts   = List(
      Option.when(hours > 0)(s"${hours}h"),
      Option.when(hours > 0 || minutes > 0)(s"${minutes}m"),
      Some(s"${seconds}s"),
      Some(s"${ms}ms")
    ).flatten
    s"\nTotal duration: ${parts.mkString(" ")}"

object Tracker {

  def logging[F[_]: Async, I](
      label: String = "",
      logInterval: Int = 10,
      showTopMember: Boolean = true,
      showTopN: Int = 1,
      showStats: Boolean = false,
      finalTopN: Int = 25
  ): F[Tracker[F, I]] =
    LoggingTracker.make(label, logInterval, showTopMember, showTopN, showStats, finalTopN)

  def markdown[F[_]: Async, I](
      label: String = "",
      logInterval: Int = 10,
      showTopMember: Boolean = true,
      showTopN: Int = 1,
      showStats: Boolean = false,
      finalTopN: Int = 25
  ): F[Tracker[F, I]] =
    MarkdownTracker.make(label, logInterval, showTopMember, showTopN, showStats, finalTopN)

  def composite[F[_]: Monad, I](trackers: Tracker[F, I]*): Tracker[F, I] =
    CompositeTracker.make(trackers*)

  def noop[F[_]: Monad, I]: F[Tracker[F, I]] =
    NoopTracker.make
}
