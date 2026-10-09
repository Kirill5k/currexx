package currexx.backtest.optimizer.reporting

import cats.effect.Async
import cats.syntax.all.*
import currexx.backtest.{OptimisationRound, StrategyCatalogue}
import currexx.backtest.optimizer.{IndicatorSearchSpace, OptimisationResult, UpgradeDecision}
import currexx.domain.signal.Indicator

import scala.concurrent.duration.FiniteDuration

/** Replays distinct baselines, final leaders, and the best searched candidate after selection has finished. */
final class OptimisationReportBuilder[F[_]: Async](
    round: OptimisationRound,
    space: IndicatorSearchSpace,
    inspect: Indicator => F[CandidateDiagnostics],
    diagnostics: RunDiagnostics[F],
    catalogue: List[StrategyCatalogue.Entry]
):
  def build(
      result: OptimisationResult,
      frozenSnapshot: RunDiagnostics.Snapshot,
      optimisationDuration: FiniteDuration
  ): F[OptimisationReport] =
    for
      started   <- Async[F].monotonic
      before    <- diagnostics.snapshot
      canonical <- Async[F].fromEither(result.finalists.map { case (indicator, training, validation) =>
        space.canonicalise(indicator).map(canonical => (canonical, training, validation))
      }.sequence)
      target   <- Async[F].fromEither(space.canonicalise(round.strategy.indicator))
      resolved <- Async[F].fromEither(space.resolveSeeds(round.extraSeeds.map(_.indicator)))
      baselines = BaselineReport("target", Some(target), None) :: resolved.map { seed =>
        val original = round.extraSeeds(seed.index)
        BaselineReport(original.name, seed.effective, Some(seed.disposition), seed.effective.exists(_ != original.indicator))
      }
      leaders  = canonical.headOption.map(_._1).toList ++ canonical.sortBy(c => -c._2.value).headOption.map(_._1).toList
      approved = result.decision match
        case UpgradeDecision.Approved(candidate, _, _) => List(candidate)
        case UpgradeDecision.RetainBase(_)             => Nil
      replay = (baselines.flatMap(_.effective) ++ leaders ++ approved ++ frozenSnapshot.bestSeen.map(_.indicator)).distinct
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
        reportWork(after),
        reportWork(before)
      ),
      optimisationDuration,
      ended - started,
      Some(result),
      round.searchObjective,
      round.upgradePolicy,
      round.scoringFunction.description
    )

  private def catalogueMatches(indicator: Indicator): List[CatalogueMatch] =
    catalogue.flatMap { entry =>
      val sameRules = entry.strategy.rules == round.strategy.rules
      if (entry.strategy.indicator == indicator) List(CatalogueMatch(entry.name, DuplicateKind.Exact, sameRules))
      else if (space.canonicalise(entry.strategy.indicator).contains(indicator))
        List(CatalogueMatch(entry.name, DuplicateKind.FixedInputsRestored, sameRules))
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

  // A diagnostic leader outside the shortlist can miss the shared selection cache. Include that post-decision work in this report,
  // while keeping the frozen validation snapshot unchanged and preserving the canonical cache shared by validation and reporting.
  private def reportWork(snapshot: RunDiagnostics.Snapshot): RunDiagnostics.Workload =
    List(RunDiagnostics.Stage.Reporting, RunDiagnostics.Stage.Validation)
      .map(stage => snapshot.workloads.getOrElse(stage, RunDiagnostics.Workload()))
      .foldLeft(RunDiagnostics.Workload()) { (total, work) =>
        RunDiagnostics.Workload(
          total.candidateRequests + work.candidateRequests,
          total.foldAttempts + work.foldAttempts,
          total.foldCompleted + work.foldCompleted,
          total.pairAttempts + work.pairAttempts,
          total.pairCompleted + work.pairCompleted
        )
      }
