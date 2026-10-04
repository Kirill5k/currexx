package currexx.backtest.optimizer.reporting

import cats.effect.Sync
import cats.syntax.all.*
import currexx.algorithms.{Parameters, ValidatedPopulation}
import currexx.algorithms.progress.{Progress, Tracker}
import currexx.domain.signal.Indicator

/** Adds comparable all-fold observations without changing the population or the generic tracker contract. */
final class ReportingTracker[F[_]: Sync](
    delegate: Tracker[F, Indicator],
    diagnostics: RunDiagnostics[F],
    foldCount: Int,
    logInterval: Int = 10
) extends Tracker[F, Indicator]:
  require(logInterval > 0, "Reporting interval must be positive")

  override def displayInitial(target: Indicator, params: Parameters[?]): F[Unit] =
    delegate.displayInitial(target, params) >> delegate.displayNote(
      "Reading progress fitness",
      OptimisationReportRenderer.progressIntro
    )

  override def displayProgress(progress: Progress[Indicator]): F[Unit] =
    delegate.displayProgress(progress) >> Sync[F].whenA(progress.currentGen % logInterval == 0) {
      diagnostics.snapshot.flatMap { snapshot =>
        delegate.displayNote(
          s"Stable progress: generation ${progress.currentGen}",
          OptimisationReportRenderer.progress(snapshot, progress.currentGen, foldCount)
        )
      }
    }

  override def displayFinal(population: ValidatedPopulation[Indicator]): F[Unit] = delegate.displayFinal(population)
  override def displayNote(title: String, lines: List[String]): F[Unit]          = delegate.displayNote(title, lines)
