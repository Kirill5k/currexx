package currexx.backtest.optimizer

import cats.effect.Async
import currexx.algorithms.{EvaluatedPopulation, Fitness}

trait ProgressTracker[F[_], T]:
  def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[T]): F[Unit]
  def displayFinal(population: EvaluatedPopulation[T], topN: Int): F[Unit]

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
      showTopN: Int = 1,
      showStats: Boolean = false
  ): F[ProgressTracker[F, T]] =
    Async[F].pure {
      new ProgressTracker[F, T]:
        override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[T]): F[Unit] =
          Async[F].whenA(currentGen % logInterval == 0) {
            val progress   = progressMsg(currentGen, maxGen)
            val topMembers = if (showTopMember && population.nonEmpty) "\n" + membersMsg(population, showTopN) else ""
            val stats = if (showStats) "\n" + statsMsg(population) else ""
            Async[F].delay(println(s"$progress$topMembers$stats"))
          }

        override def displayFinal(population: EvaluatedPopulation[T], topN: Int): F[Unit] =
          Async[F].delay(println(s"Final top $topN members:\n${membersMsg(population, topN)}\n${statsMsg(population)}"))
    }
