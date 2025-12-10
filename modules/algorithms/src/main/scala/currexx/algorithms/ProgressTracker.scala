package currexx.algorithms

import cats.effect.Async

trait ProgressTracker[F[_], I]:
  def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit]
  def displayFinal(population: EvaluatedPopulation[I]): F[Unit]

object ProgressTracker:

  private def progressMsg(currentGen: Int, maxGen: Int): String =
    s"Generation $currentGen out of $maxGen"

  private def memberMsg[I](idx: Int, individual: I, fitness: Fitness): String =
    s"#${idx + 1}: $fitness - $individual"

  private def membersMsg[I](population: EvaluatedPopulation[I], topN: Int): String =
    population
      .take(topN)
      .zipWithIndex
      .map { case ((individual, fitness), idx) => s"  ${memberMsg(idx, individual, fitness)}" }
      .mkString("\n")

  private def statsMsg[I](population: EvaluatedPopulation[I]): String =
    val fitnesses = population.map(_._2.value)
    val avg       = fitnesses.sum / fitnesses.size
    val best      = population.head._2.value
    val worst     = population.last._2.value
    s"Stats: Best=$best, Avg=$avg, Worst=$worst"

  def make[F[_]: Async, I](
      logInterval: Int = 10,
      showTopMember: Boolean = true,
      showTopN: Int = 1,
      showStats: Boolean = false,
      finalTopN: Int = 25
  ): F[ProgressTracker[F, I]] =
    Async[F].pure {
      new ProgressTracker[F, I]:
        override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit] =
          Async[F].whenA(currentGen % logInterval == 0) {
            val progress   = progressMsg(currentGen, maxGen)
            val topMembers = if (showTopMember && population.nonEmpty) "\n" + membersMsg(population, showTopN) else ""
            val stats = if (showStats) "\n" + statsMsg(population) else ""
            Async[F].delay(println(s"$progress$topMembers$stats"))
          }

        override def displayFinal(population: EvaluatedPopulation[I]): F[Unit] =
          Async[F].delay(println(s"Final top $finalTopN members:\n${membersMsg(population, finalTopN)}\n${statsMsg(population)}"))
    }

