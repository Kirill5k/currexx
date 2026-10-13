package currexx.backtest.optimizer

import currexx.algorithms.{EvaluationPhase, Fitness, memoize}
import currexx.algorithms.operators.Evaluator
import currexx.backtest.OrderStats
import currexx.backtest.TestStrategy
import cats.MonadThrow
import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

/** Scores a candidate from its per-fold backtests, withholding one search fold per generation.
  *
  * A candidate scored on the same folds every generation can be selected for fitting them, and a hundred generations is long enough to do
  * it thoroughly. Rotating which fold is withheld means no candidate is ever ranked on the whole corpus during the search, so a shape that
  * only works because of one regime loses the generations where that regime is the one held back.
  *
  * Cheap because the withholding is in the aggregation and not in the backtest: `foldScores` is expected to be memoised and to return every
  * fold, so a rotation costs a geometric mean over a shorter list and an elite that survives twenty generations is still backtested once.
  * `EvaluationPhase.Rescore` counts every fold, which is what makes the figure a run finally reports comparable with another run's.
  */
final class FoldRotatingEvaluator[F[_]: MonadThrow](
    foldScores: TestStrategy => F[List[Double]],
    foldCount: Int,
    canonicalise: TestStrategy => Either[Throwable, TestStrategy] = candidate => Right(candidate),
    observer: Option[FoldRotatingEvaluator.Observer[F]] = None
) extends Evaluator[F, TestStrategy]:

  override def evaluateIndividual(candidate: TestStrategy, phase: EvaluationPhase): F[(TestStrategy, Fitness)] =
    evaluate(candidate, phase, (_, _) => MonadThrow[F].unit)

  /** A run-local view observes successful searches while sharing the existing fold-score cache and diagnostics. */
  def observingSearch(callback: (TestStrategy, Fitness) => F[Unit]): Evaluator[F, TestStrategy] =
    val onSearch = (candidate: TestStrategy, scores: List[Double]) => callback(candidate, Fitness(FoldAggregation.combine(scores)))
    new Evaluator[F, TestStrategy] {
      override def evaluateIndividual(candidate: TestStrategy, phase: EvaluationPhase): F[(TestStrategy, Fitness)] =
        phase match
          case EvaluationPhase.Search(_) => evaluate(candidate, phase, onSearch)
          case EvaluationPhase.Rescore   => FoldRotatingEvaluator.this.evaluateIndividual(candidate, phase)
    }

  private def evaluate(
      candidate: TestStrategy,
      phase: EvaluationPhase,
      onEvaluated: (TestStrategy, List[Double]) => F[Unit]
  ): F[(TestStrategy, Fitness)] =
    MonadThrow[F].fromEither(canonicalise(candidate)).flatMap { canonical =>
      observer.traverse_(_.requested(phase)).flatMap { _ =>
        foldScores(canonical)
          .flatTap(scores => observer.traverse_(_.observed(canonical, phase, scores)))
          .flatTap(scores => onEvaluated(canonical, scores))
          .map(scores => canonical -> Fitness(FoldAggregation.combineExcluding(scores, withheldFold(phase))))
      }
    }

  /** Which fold this phase does not get to count, if any. */
  private[optimizer] def withheldFold(phase: EvaluationPhase): Option[Int] = phase match
    case EvaluationPhase.Rescore => None
    // Withholding the only fold there is would score every candidate on nothing and rank them all equal, so a corpus with none to spare
    // is read whole. Two is the fewest that can lose one and still say something.
    case EvaluationPhase.Search(_) if foldCount <= 1 => None
    case EvaluationPhase.Search(generation)          => Some(math.floorMod(generation, foldCount))

object FoldRotatingEvaluator:

  /** Callbacks observe existing evaluation work without changing scores or retaining backtest histories. */
  final case class Observer[F[_]](
      requested: EvaluationPhase => F[Unit],
      observed: (TestStrategy, EvaluationPhase, List[Double]) => F[Unit],
      computationStarted: TestStrategy => F[Unit],
      computationCompleted: F[Unit],
      foldStarted: F[Unit],
      foldCompleted: F[Unit]
  )

  /** Cache only each fold's score: retaining OrderStats would keep every candidate's trades and equity curves for the entire search. Score
    * a fold before starting the next one so completed folds' histories can also be collected while a candidate is still running.
    */
  def cached[F[_]: Concurrent](
      backtests: List[TestStrategy => F[List[OrderStats]]],
      scoringFunction: ScoringFunction,
      canonicalise: TestStrategy => Either[Throwable, TestStrategy] = candidate => Right(candidate),
      observer: Option[Observer[F]] = None
  ): F[FoldRotatingEvaluator[F]] =
    cachedScores(backtests.map(run => candidate => run(candidate).map(scoringFunction.score)), canonicalise, observer)

  /** Accept already reduced fold computations so an objective can use compact baseline measurements without retaining candidate histories.
    * Initial scores are measurements prepared before search, with their actual work recorded by the caller.
    */
  private[optimizer] def cachedScores[F[_]: Concurrent](
      computations: List[TestStrategy => F[Double]],
      canonicalise: TestStrategy => Either[Throwable, TestStrategy],
      observer: Option[Observer[F]],
      initialScores: Map[TestStrategy, List[Double]] = Map.empty
  ): F[FoldRotatingEvaluator[F]] =
    memoize[F, TestStrategy, List[Double]] { candidate =>
      observer.traverse_(_.computationStarted(candidate)).flatMap { _ =>
        computations
          .traverse { compute =>
            observer
              .traverse_(_.foldStarted)
              .flatMap { _ =>
                compute(candidate)
                  .flatTap(_ => observer.traverse_(_.foldCompleted))
              }
          }
          .flatTap(_ => observer.traverse_(_.computationCompleted))
      }
    }.map { scores =>
      val cached = (candidate: TestStrategy) => initialScores.get(candidate).fold(scores(candidate))(Concurrent[F].pure)
      FoldRotatingEvaluator(cached, computations.size, canonicalise, observer)
    }
