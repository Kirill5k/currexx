package currexx.backtest.optimizer.reporting

import cats.Monad
import cats.syntax.all.*
import currexx.algorithms.{Parameters, ValidatedPopulation}
import currexx.algorithms.progress.{Progress, Tracker}
import currexx.domain.signal.Indicator

/** Renders progress and completed diagnostics through the existing output sinks. */
final class ReportingTracker[F[_]: Monad](
    delegate: Tracker[F, Indicator],
    diagnostics: RunDiagnostics[F],
    foldCount: Int,
    logInterval: Int = 10
) extends Tracker[F, Indicator]:
  require(logInterval > 0, "Reporting interval must be positive")

  override def displayInitial(target: Indicator, params: Parameters[?]): F[Unit] =
    delegate.displayInitial(target, params) >>
      delegate.displayNote("Reading progress fitness", OptimisationReportRenderer.progressIntro)

  override def displayProgress(progress: Progress[Indicator]): F[Unit] =
    delegate.displayProgress(progress) >> Monad[F].whenA(progress.currentGen % logInterval == 0) {
      diagnostics.snapshot.flatMap { snapshot =>
        delegate.displayNote(
          s"Stable progress: generation ${progress.currentGen}",
          OptimisationReportRenderer.progress(snapshot, progress.currentGen, foldCount)
        )
      }
    }

  override def displayFinal(population: ValidatedPopulation[Indicator]): F[Unit] = delegate.displayFinal(population)
  override def displayNote(title: String, lines: List[String]): F[Unit]          = delegate.displayNote(title, lines)

  def displayReport(report: OptimisationReport): F[Unit] =
    OptimisationReportRenderer.sections(report).traverse_ { case (title, lines) =>
      delegate.displayNote(title, lines)
    }

  def displayReportFailure(roundName: String, error: Throwable): F[Unit] =
    val (title, lines) = OptimisationReportRenderer.incompleteDiagnostics(roundName, error)
    delegate.displayNote(title, lines)
