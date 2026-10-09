package currexx.backtest.optimizer.reporting

import cats.Monad
import cats.syntax.all.*
import currexx.algorithms.{Parameters, ValidatedPopulation}
import currexx.algorithms.progress.{Progress, Tracker}
import currexx.backtest.optimizer.SearchObjectiveConfig
import currexx.domain.signal.Indicator

/** Renders progress and completed diagnostics through the existing output sinks. */
final class ReportingTracker[F[_]: Monad](
    delegate: Tracker[F, Indicator],
    diagnostics: RunDiagnostics[F],
    foldCount: Int,
    logInterval: Int = 10,
    searchObjective: SearchObjectiveConfig = SearchObjectiveConfig.Current
) extends Tracker[F, Indicator]:

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

  override def displayFinal(population: ValidatedPopulation[Indicator]): F[Unit] =
    Monad[F].unit

  // Generic algorithms finish before the upgrade policy runs. Delay final output until the completed decision is available.
  def displayCompletedRankings(population: ValidatedPopulation[Indicator]): F[Unit] =
    if (searchObjective == SearchObjectiveConfig.Current) delegate.displayFinal(population)
    else
      delegate.displayNote(
        "Final rankings (separate objectives)",
        List("Search fitness uses the baseline-relative objective. Absolute validation fitness uses the quality scorer.") :::
          population.toList.zipWithIndex.map { case ((indicator, training, validation), index) =>
            f"#${index + 1}: search=${training.value}%.6f; absolute validation=${validation.value}%.6f; indicator=$indicator"
          }
      )
  override def displayNote(title: String, lines: List[String]): F[Unit] = delegate.displayNote(title, lines)

  def displayReport(report: OptimisationReport): F[Unit] =
    OptimisationReportRenderer.sections(report).traverse_ { case (title, lines) =>
      delegate.displayNote(title, lines)
    }

  def displayReportFailure(roundName: String, error: Throwable): F[Unit] =
    val (title, lines) = OptimisationReportRenderer.incompleteDiagnostics(roundName, error)
    delegate.displayNote(title, lines)
