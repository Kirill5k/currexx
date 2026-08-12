package currexx.algorithms.progress

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import currexx.algorithms.{EvaluatedPopulation, Parameters, ValidatedPopulation}

import java.time.Instant

final class LoggingTracker[F[_], I] private (
    label: String,
    startTimeRef: Ref[F, Option[Instant]],
    logInterval: Int,
    showTopMember: Boolean,
    showTopN: Int,
    showStats: Boolean,
    finalTopN: Int
)(using
    F: Async[F]
) extends Tracker[F, I]:

  override def displayInitial(target: I, params: Parameters.GA): F[Unit] =
    for
      now <- Async[F].realTimeInstant
      _   <- startTimeRef.set(Some(now))
      header = s"Starting GA${if (label.nonEmpty) s" round $label" else ""} at $now"
      _ <- Async[F].delay(println(s"$header\nTarget: $target\nParameters: $params"))
    yield ()

  override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit] =
    Async[F].whenA(currentGen % logInterval == 0) {
      val progress   = progressMsg(currentGen, maxGen)
      val topMembers = if (showTopMember && population.nonEmpty) "\n" + membersMsg(population, showTopN) else ""
      val stats      = if (showStats) "\n" + statsMsg(population) else ""
      Async[F].delay(println(s"$progress$topMembers$stats"))
    }

  override def displayFinal(population: ValidatedPopulation[I]): F[Unit] =
    for
      now       <- Async[F].realTimeInstant
      startTime <- startTimeRef.get
      duration = startTime.map(start => durationMsg(start, now)).getOrElse("")
      stats    = validatedStatsMsg(population)
      summary  = validationSummaryMsg(population)
      members  = validatedMembersMsg(population, finalTopN)
      _ <- Async[F].delay(println(s"$summary\nFinal top $finalTopN members:\n$members\n$stats$duration\n"))
    yield ()

  override def displayNote(title: String, lines: List[String]): F[Unit] =
    Async[F].delay(println((title :: lines).mkString("\n")))

object LoggingTracker:
  def make[F[_]: Async, I](
      label: String = "",
      logInterval: Int = 10,
      showTopMember: Boolean = true,
      showTopN: Int = 1,
      showStats: Boolean = false,
      finalTopN: Int = 25
  ): F[Tracker[F, I]] =
    Ref
      .of[F, Option[Instant]](None)
      .map { startTimeRef =>
        new LoggingTracker[F, I](label, startTimeRef, logInterval, showTopMember, showTopN, showStats, finalTopN)
      }
