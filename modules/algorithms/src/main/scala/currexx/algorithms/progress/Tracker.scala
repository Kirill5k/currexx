package currexx.algorithms.progress

import cats.Monad
import cats.effect.Async
import currexx.algorithms.{EvaluatedPopulation, Fitness, Parameters}

import java.time.Instant
import scala.concurrent.duration.*

trait Tracker[F[_], I]:
  def displayInitial(target: I, params: Parameters.GA): F[Unit]
  def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit]
  def displayFinal(population: EvaluatedPopulation[I]): F[Unit]

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

  protected def statsMsg(population: EvaluatedPopulation[I]): String =
    val fitnesses = population.map(_._2.value)
    val avg       = fitnesses.sum / fitnesses.size
    val best      = population.head._2.value
    val worst     = population.last._2.value
    s"Stats: Best=$best, Avg=$avg, Worst=$worst"

  protected def durationMsg(start: Instant, end: Instant): String =
    val duration = (end.toEpochMilli - start.toEpochMilli).millis.toCoarsest
    s"\nTotal duration: $duration"

object Tracker {

  def logging[F[_]: Async, I](
      logInterval: Int = 10,
      showTopMember: Boolean = true,
      showTopN: Int = 1,
      showStats: Boolean = false,
      finalTopN: Int = 25
  ): F[Tracker[F, I]] =
    LoggingTracker.make(logInterval, showTopMember, showTopN, showStats, finalTopN)

  def noop[F[_]: Monad, I]: F[Tracker[F, I]] =
    Monad[F].pure {
      new Tracker[F, I]:
        override def displayInitial(target: I, params: Parameters.GA): F[Unit]                                  = Monad[F].unit
        override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit] = Monad[F].unit
        override def displayFinal(population: EvaluatedPopulation[I]): F[Unit]                                  = Monad[F].unit
    }
}
