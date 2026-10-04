package currexx.backtest.optimizer.reporting

import cats.effect.{Ref, Sync}
import cats.syntax.functor.*
import currexx.algorithms.EvaluationPhase
import currexx.backtest.optimizer.IndicatorObjective.FoldAggregation
import currexx.backtest.optimizer.FoldRotatingEvaluator
import currexx.domain.signal.Indicator

/** Observes a run without participating in selection or retaining its backtest histories. */
final class RunDiagnostics[F[_]] private (state: Ref[F, RunDiagnostics.State])(using F: Sync[F]) {
  import RunDiagnostics.*

  def snapshot: F[Snapshot] = state.get.map(_.snapshot)

  def evaluatorObserver: FoldRotatingEvaluator.Observer[F] = FoldRotatingEvaluator.Observer(
    requested = requested,
    observed = observed,
    computationStarted = computationStarted,
    computationCompleted = computationCompleted,
    foldStarted = foldStarted(Stage.Search),
    foldCompleted = foldCompleted(Stage.Search)
  )

  def requested(phase: EvaluationPhase): F[Unit] =
    state.update { current =>
      val snapshot = current.snapshot
      val counted  = phase match {
        case EvaluationPhase.Search(_) => snapshot.copy(searchRequests = snapshot.searchRequests + 1)
        case EvaluationPhase.Rescore   => snapshot.copy(rescoreRequests = snapshot.rescoreRequests + 1)
      }
      current.copy(snapshot = counted.withWorkload(Stage.Search)(w => w.copy(candidateRequests = w.candidateRequests + 1)))
    }

  /** Every observation uses all cached fold scores, even when selection withholds a fold in this generation. */
  def observed(indicator: Indicator, phase: EvaluationPhase, foldScores: List[Double]): F[Unit] = phase match {
    case EvaluationPhase.Rescore            => F.unit
    case EvaluationPhase.Search(generation) =>
      val fitness = FoldAggregation.combine(foldScores)
      state.update { current =>
        val snapshot  = current.snapshot
        val first     = snapshot.firstSeen.get(indicator).fold(generation)(_.min(generation))
        val seen      = snapshot.firstSeen.updated(indicator, first)
        val discovery = Discovery(indicator, fitness, first)
        val best      = snapshot.bestSeen match {
          case None           => discovery
          case Some(previous) =>
            if (better(discovery, previous)) discovery else previous
        }
        current.copy(snapshot =
          snapshot.copy(
            firstSeen = seen,
            bestSeen = Some(best),
            successfulSearchRequests = snapshot.successfulSearchRequests + 1,
            distinctSearchCandidates = seen.size
          )
        )
      }
  }

  /** Called inside the memoized computation, so concurrent cache waiters do not count as expensive attempts. */
  def computationStarted(indicator: Indicator): F[Unit] =
    state.update { current =>
      val computed = current.computed + indicator
      current.copy(
        computed = computed,
        snapshot = current.snapshot.copy(
          computationAttempts = current.snapshot.computationAttempts + 1,
          uniqueComputedCandidates = computed.size
        )
      )
    }

  def computationCompleted: F[Unit] =
    state.update(current =>
      current.copy(snapshot = current.snapshot.copy(completedComputations = current.snapshot.completedComputations + 1))
    )

  def candidateRequested(stage: Stage): F[Unit] =
    updateWorkload(stage)(w => w.copy(candidateRequests = w.candidateRequests + 1))

  def foldStarted(stage: Stage): F[Unit]   = updateWorkload(stage)(w => w.copy(foldAttempts = w.foldAttempts + 1))
  def foldCompleted(stage: Stage): F[Unit] = updateWorkload(stage)(w => w.copy(foldCompleted = w.foldCompleted + 1))
  def pairStarted(stage: Stage): F[Unit]   = updateWorkload(stage)(w => w.copy(pairAttempts = w.pairAttempts + 1))
  def pairCompleted(stage: Stage): F[Unit] = updateWorkload(stage)(w => w.copy(pairCompleted = w.pairCompleted + 1))

  private def updateWorkload(stage: Stage)(f: Workload => Workload): F[Unit] =
    state.update(current => current.copy(snapshot = current.snapshot.withWorkload(stage)(f)))
}

object RunDiagnostics {
  enum Stage {
    case Search, Validation, Reporting, Backtest
  }

  final case class Workload(
      candidateRequests: Long = 0L,
      foldAttempts: Long = 0L,
      foldCompleted: Long = 0L,
      pairAttempts: Long = 0L,
      pairCompleted: Long = 0L
  )

  final case class Discovery(indicator: Indicator, fitness: Double, generation: Int)

  final case class Snapshot(
      firstSeen: Map[Indicator, Int] = Map.empty,
      bestSeen: Option[Discovery] = None,
      workloads: Map[Stage, Workload] = Stage.values.map(_ -> Workload()).toMap,
      searchRequests: Long = 0L,
      rescoreRequests: Long = 0L,
      successfulSearchRequests: Long = 0L,
      distinctSearchCandidates: Int = 0,
      computationAttempts: Long = 0L,
      completedComputations: Long = 0L,
      uniqueComputedCandidates: Int = 0
  ) {

    /** Includes waiting on an in-flight computation. Read after evaluation settles, before report replays. */
    def cacheReuses: Long = searchRequests + rescoreRequests - computationAttempts

    private[reporting] def withWorkload(stage: Stage)(f: Workload => Workload): Snapshot =
      copy(workloads = workloads.updated(stage, f(workloads.getOrElse(stage, Workload()))))
  }

  final private case class State(snapshot: Snapshot = Snapshot(), computed: Set[Indicator] = Set.empty)

  // Equal scores are ordered independently of which parallel backtest completed first.
  private def better(candidate: Discovery, previous: Discovery): Boolean =
    candidate.fitness > previous.fitness ||
      (candidate.fitness == previous.fitness &&
        (candidate.generation < previous.generation ||
          (candidate.generation == previous.generation && candidate.indicator.toString < previous.indicator.toString)))

  def make[F[_]: Sync]: F[RunDiagnostics[F]] =
    Ref.of[F, State](State()).map(new RunDiagnostics(_))
}
