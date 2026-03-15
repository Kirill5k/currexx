package currexx.algorithms.progress

import cats.Monad
import cats.effect.{Async, Ref}
import cats.syntax.all.*
import currexx.algorithms.{EvaluatedPopulation, Fitness, Parameters}

import java.time.Instant
import scala.concurrent.duration.*

trait Tracker[F[_], I]:
  def displayInitial(target: I, params: Parameters.GA): F[Unit]
  def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit]
  def displayFinal(population: EvaluatedPopulation[I]): F[Unit]

object Tracker {

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

  private def durationMsg(start: Option[Instant], end: Instant): String =
    start
      .map { s =>
        val duration = (end.toEpochMilli - s.toEpochMilli).millis.toCoarsest
        s"\nTotal duration: $duration"
      }
      .getOrElse("")

  def make[F[_]: Async, I](
      logInterval: Int = 10,
      showTopMember: Boolean = true,
      showTopN: Int = 1,
      showStats: Boolean = false,
      finalTopN: Int = 25
  ): F[Tracker[F, I]] =
    Ref.of[F, Option[Instant]](None).map { startTimeRef =>
      new Tracker[F, I]:
        override def displayInitial(target: I, params: Parameters.GA): F[Unit] =
          for
            now <- Async[F].realTimeInstant
            _   <- startTimeRef.set(Some(now))
            _   <- Async[F].delay(println(s"Starting GA at $now\nTarget: $target\nParameters: $params"))
          yield ()

        override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit] =
          Async[F].whenA(currentGen % logInterval == 0) {
            val progress   = progressMsg(currentGen, maxGen)
            val topMembers = if (showTopMember && population.nonEmpty) "\n" + membersMsg(population, showTopN) else ""
            val stats      = if (showStats) "\n" + statsMsg(population) else ""
            Async[F].delay(println(s"$progress$topMembers$stats"))
          }

        override def displayFinal(population: EvaluatedPopulation[I]): F[Unit] =
          for
            now       <- Async[F].realTimeInstant
            startTime <- startTimeRef.get
            _ <- Async[F].delay(
              println(s"Final top $finalTopN members:\n${membersMsg(population, finalTopN)}\n${statsMsg(population)}${durationMsg(startTime, now)}")
            )
          yield ()
    }

  def noop[F[_]: Monad, I]: F[Tracker[F, I]] =
    Monad[F].pure {
      new Tracker[F, I]:
        override def displayInitial(target: I, params: Parameters.GA): F[Unit]                                  = Monad[F].unit
        override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit] = Monad[F].unit
        override def displayFinal(population: EvaluatedPopulation[I]): F[Unit]                                  = Monad[F].unit
    }
}
