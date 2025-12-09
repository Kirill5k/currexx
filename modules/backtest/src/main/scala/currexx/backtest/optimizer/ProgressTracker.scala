package currexx.backtest.optimizer

import cats.effect.Async
import currexx.algorithms.{EvaluatedPopulation, Fitness}

trait ProgressTracker[F[_], T]:
  def update(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[T]): F[Unit]

object ProgressTracker:

  private def progressMsg(currentGen: Int, maxGen: Int): String =
    s"Generation $currentGen out of $maxGen"

  private def memberMsg[T](idx: Int, individual: T, fitness: Fitness): String =
    s"#${idx + 1}: $fitness - $individual"

  private def membersMsg[T](population: EvaluatedPopulation[T], topN: Int): String =
    population
      .take(topN)
      .zipWithIndex
      .map { case ((individual, fitness), idx) => s"  ${memberMsg(idx, individual, fitness)}" }
      .mkString("\n")

  private def statsMsg[T](population: EvaluatedPopulation[T]): String =
    val fitnesses = population.map(_._2.value)
    val avg       = fitnesses.sum / fitnesses.size
    val best      = population.head._2.value
    val worst     = population.last._2.value
    s"Stats: Best=$best, Avg=$avg, Worst=$worst"

  def make[F[_]: Async, T](
      logInterval: Int = 10,
      showTopMember: Boolean = true,
      showTopN: Int = 1
  ): F[ProgressTracker[F, T]] =
    Async[F].pure {
      new ProgressTracker[F, T]:
        override def update(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[T]): F[Unit] =
          Async[F].whenA(currentGen % logInterval == 0) {
            val progress   = progressMsg(currentGen, maxGen)
            val topMembers = if (showTopMember && population.nonEmpty) membersMsg(population, showTopN) else ""
            Async[F].delay(println(s"$progress\n$topMembers"))
          }
    }

  def simple[F[_]: Async, T](logInterval: Int = 10): F[ProgressTracker[F, T]] =
    Async[F].pure {
      new ProgressTracker[F, T]:
        override def update(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[T]): F[Unit] =
          Async[F].whenA(currentGen % logInterval == 0) {
            Async[F].delay(println(progressMsg(currentGen, maxGen)))
          }
    }

  def detailed[F[_]: Async, T](
      logInterval: Int = 10,
      showTopN: Int = 5,
      showStats: Boolean = true
  ): F[ProgressTracker[F, T]] =
    Async[F].pure {
      new ProgressTracker[F, T]:
        override def update(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[T]): F[Unit] =
          Async[F].whenA(currentGen % logInterval == 0) {
            val progress = progressMsg(currentGen, maxGen)
            val members  = membersMsg(population, showTopN)
            val stats    = if (showStats) statsMsg(population) else ""
            Async[F].delay(println(s"$progress\n$members\n$stats"))
          }
    }
