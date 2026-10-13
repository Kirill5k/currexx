package currexx.backtest.optimizer.reporting

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.backtest.{StrategyCatalogue, TestStrategy}
import currexx.backtest.optimizer.{OptimisationResult, OptimisationRound, OptimisationSearchSpace, OptimisationTarget, UpgradeDecision}

import scala.concurrent.duration.FiniteDuration

/** Replays distinct baselines, final leaders, and the best searched candidate after selection has finished. */
final class OptimisationReportBuilder[F[_]](
    round: OptimisationRound,
    space: OptimisationSearchSpace,
    inspect: TestStrategy => F[CandidateDiagnostics],
    diagnostics: RunDiagnostics[F],
    catalogue: List[StrategyCatalogue.Entry]
)(using F: Async[F]):
  def build(
      result: OptimisationResult,
      frozenSnapshot: RunDiagnostics.Snapshot,
      optimisationDuration: FiniteDuration
  ): F[OptimisationReport] =
    for
      started   <- F.monotonic
      before    <- diagnostics.snapshot
      canonical <- F.fromEither(result.finalists.map { case (candidate, training, validation) =>
        space.canonicalise(candidate).map(canonical => (canonical, training, validation))
      }.sequence)
      target   <- F.fromEither(space.canonicalise(round.strategy))
      resolved <- F.fromEither(space.resolveSeeds(round.extraSeeds.map(_.strategy)))
      baselines = BaselineReport("target", Some(target), None) :: resolved.map { seed =>
        val original = round.extraSeeds(seed.index)
        BaselineReport(
          original.name,
          seed.effective,
          Some(seed.disposition),
          seed.effective.exists(fixedInputsRestored(original.strategy, _)),
          seed.reason
        )
      }
      leaders  = canonical.headOption.map(_._1).toList ++ canonical.sortBy(c => -c._2.value).headOption.map(_._1).toList
      approved = result.decision match
        case UpgradeDecision.Approved(candidate, _, _) => List(candidate)
        case UpgradeDecision.RetainBase(_)             => Nil
      replay = (baselines.flatMap(_.effective) ++ leaders ++ approved ++ frozenSnapshot.bestSeen.map(_.candidate)).distinct
      measurements      <- replay.traverse(candidate => inspect(candidate).map(candidate -> _))
      after             <- diagnostics.snapshot
      resolvedCatalogue <- F.fromEither(space.resolveSeeds(catalogue.map(_.strategy)))
      projectedCatalogue = catalogue.zip(resolvedCatalogue).flatMap { case (entry, resolved) =>
        resolved.effective.map(effective => (entry, effective, resolved.usedBaseRules))
      }
      matches = (replay ++ canonical.map(_._1)).distinct
        .map(candidate => candidate -> catalogueMatches(candidate, projectedCatalogue))
        .toMap
      ended <- F.monotonic
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
      round.scoringFunction.description,
      target = round.target,
      baseCandidate = round.strategy,
      fixedIndicators = space.fixedIndicators,
      ruleBounds = round.ruleBounds
    )

  private def fixedInputsRestored(original: TestStrategy, effective: TestStrategy): Boolean = round.target match
    case OptimisationTarget.Indicator => original.indicator != effective.indicator
    case OptimisationTarget.Strategy  => original.rules != effective.rules
    case OptimisationTarget.Both      => original != effective

  private def catalogueMatches(
      candidate: TestStrategy,
      projectedCatalogue: List[(StrategyCatalogue.Entry, TestStrategy, Boolean)]
  ): List[CatalogueMatch] =
    projectedCatalogue.flatMap { case (entry, effective, usedBaseRules) =>
      if (entry.strategy == candidate) List(CatalogueMatch(entry.name, DuplicateKind.Exact))
      else if (effective == candidate)
        val scope = round.target match
          case OptimisationTarget.Both if usedBaseRules                                       => CatalogueScope.IndicatorsOnly
          case OptimisationTarget.Indicator if entry.strategy.rules != candidate.rules        => CatalogueScope.IndicatorsOnly
          case OptimisationTarget.Strategy if entry.strategy.indicator != candidate.indicator => CatalogueScope.RulesOnly
          case _                                                                              => CatalogueScope.FullCandidate
        val kind =
          if (fixedInputsRestored(entry.strategy, candidate)) DuplicateKind.FixedInputsRestored
          else DuplicateKind.Exact
        List(CatalogueMatch(entry.name, kind, scope))
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
