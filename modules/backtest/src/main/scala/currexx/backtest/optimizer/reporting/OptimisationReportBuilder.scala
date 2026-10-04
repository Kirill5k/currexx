package currexx.backtest.optimizer.reporting

import cats.effect.Async
import cats.syntax.all.*
import currexx.algorithms.ValidatedPopulation
import currexx.backtest.{OptimisationRound, StrategyCatalogue}
import currexx.backtest.optimizer.IndicatorSearchSpace
import currexx.domain.signal.Indicator

import scala.concurrent.duration.FiniteDuration

/** Replays only the baselines and two leaders after selection has finished. */
final class OptimisationReportBuilder[F[_]: Async](
    round: OptimisationRound,
    space: IndicatorSearchSpace,
    inspect: Indicator => F[CandidateDiagnostics],
    diagnostics: RunDiagnostics[F],
    catalogue: List[StrategyCatalogue.Entry]
):
  def build(
      finalists: ValidatedPopulation[Indicator],
      frozenSnapshot: RunDiagnostics.Snapshot,
      optimisationDuration: FiniteDuration
  ): F[OptimisationReport] =
    for
      started   <- Async[F].monotonic
      before    <- diagnostics.snapshot
      canonical <- Async[F].fromEither(finalists.map { case (indicator, training, validation) =>
        space.canonicalise(indicator).map(canonical => (canonical, training, validation))
      }.sequence)
      target   <- Async[F].fromEither(space.canonicalise(round.strategy.indicator))
      resolved <- Async[F].fromEither(space.resolveSeeds(round.extraSeeds.map(_.indicator)))
      baselines = BaselineReport("target", Some(target), None) :: resolved.map { seed =>
        val original = round.extraSeeds(seed.index)
        BaselineReport(original.name, seed.effective, Some(seed.disposition), seed.effective.exists(_ != original.indicator))
      }
      leaders = canonical.headOption.map(_._1).toList ++ canonical.sortBy(c => -c._2.value).headOption.map(_._1).toList
      replay  = (baselines.flatMap(_.effective) ++ leaders).distinct
      measurements <- replay.traverse(indicator => inspect(indicator).map(indicator -> _))
      after        <- diagnostics.snapshot
      matches = (replay ++ canonical.map(_._1)).distinct.map(indicator => indicator -> catalogueMatches(indicator)).toMap
      ended <- Async[F].monotonic
    yield OptimisationReport(
      round.name,
      round.corpus,
      canonical,
      baselines,
      measurements.toMap,
      matches,
      frozenSnapshot,
      difference(
        after.workloads.getOrElse(RunDiagnostics.Stage.Reporting, RunDiagnostics.Workload()),
        before.workloads.getOrElse(RunDiagnostics.Stage.Reporting, RunDiagnostics.Workload())
      ),
      optimisationDuration,
      ended - started
    )

  private def catalogueMatches(indicator: Indicator): List[CatalogueMatch] =
    catalogue.flatMap { entry =>
      if (entry.strategy.rules != round.strategy.rules) Nil
      else if (entry.strategy.indicator == indicator) List(CatalogueMatch(entry.name, DuplicateKind.Exact))
      else if (space.canonicalise(entry.strategy.indicator).contains(indicator))
        List(CatalogueMatch(entry.name, DuplicateKind.FixedInputsRestored))
      else Nil
    }

  private def difference(after: RunDiagnostics.Workload, before: RunDiagnostics.Workload): RunDiagnostics.Workload =
    RunDiagnostics.Workload(
      after.candidateRequests - before.candidateRequests,
      after.foldAttempts - before.foldAttempts,
      after.foldCompleted - before.foldCompleted,
      after.pairAttempts - before.pairAttempts,
      after.pairCompleted - before.pairCompleted
    )
