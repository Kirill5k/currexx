package currexx.algorithms.progress

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import currexx.algorithms.{EvaluatedPopulation, Parameters}
import fs2.Stream
import fs2.io.file.{Files, Flags, Path}

import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.Instant

final class MarkdownTracker[F[_], I] private (
    path: Path,
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
      now <- F.realTimeInstant
      _   <- startTimeRef.set(Some(now))
      content =
        s"""# Genetic Algorithm Run
           |
           |**Started at:** $now
           |**Target:** $target
           |**Parameters:** $params
           |
           |## Progress
           |""".stripMargin
      _ <- writeToFile(content, append = false)
    yield ()

  override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit] =
    F.whenA(currentGen % logInterval == 0) {
      val progress   = s"### ${progressMsg(currentGen, maxGen)}"
      val topMembers = if (showTopMember && population.nonEmpty) "\n\n**Top Members:**\n\n" + membersMsg(population, showTopN) else ""
      val stats      = if (showStats) "\n\n**Stats:**\n" + statsMsg(population) else ""
      writeToFile(s"\n$progress$topMembers$stats\n", append = true)
    }

  override def displayFinal(population: EvaluatedPopulation[I]): F[Unit] =
    for
      now       <- F.realTimeInstant
      startTime <- startTimeRef.get
      duration = startTime.map(start => durationMsg(start, now)).getOrElse("")
      stats    = statsMsg(population)
      members  = membersMsg(population, finalTopN)
      content =
        s"""|
            |## Final Results
            |
            |**Top $finalTopN members:**
            |
            |$members
            |
            |**Stats:**
            |$stats
            |
            |**Duration:**
            |$duration
            |""".stripMargin
      _ <- writeToFile(content, append = true)
    yield ()

  override protected def membersMsg(population: EvaluatedPopulation[I], topN: Int): String =
    population
      .take(topN)
      .zipWithIndex
      .map { case ((individual, fitness), idx) => s"* #${idx + 1}: $fitness - `$individual`" }
      .mkString("\n")

  private def writeToFile(content: String, append: Boolean): F[Unit] =
    val files = Files.forAsync[F]
    path.parent.traverse(files.createDirectories).flatMap { _ =>
      Stream
        .emit(content)
        .through(fs2.text.utf8.encode)
        .through(files.writeAll(path, if (append) Flags.Append else Flags.Write))
        .compile
        .drain
    }

object MarkdownTracker:
  private val Formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd-HHmm").withZone(ZoneId.systemDefault())

  def make[F[_]: Async, I](
      logInterval: Int = 10,
      showTopMember: Boolean = true,
      showTopN: Int = 1,
      showStats: Boolean = false,
      finalTopN: Int = 25
  ): F[Tracker[F, I]] =
    for
      now          <- Async[F].realTimeInstant
      startTimeRef <- Ref.of[F, Option[Instant]](None)
      fileName = s"ga-optimisation-${Formatter.format(now)}.md"
      path     = Path("optimisation-results") / fileName
    yield new MarkdownTracker[F, I](path, startTimeRef, logInterval, showTopMember, showTopN, showStats, finalTopN)
