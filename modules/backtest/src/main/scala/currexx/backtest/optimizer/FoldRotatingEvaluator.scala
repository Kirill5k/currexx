package currexx.backtest.optimizer

import cats.MonadThrow
import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.operators.Evaluator
import currexx.algorithms.{EvaluationPhase, Fitness, memoize}
import currexx.backtest.OrderStats
import currexx.domain.signal.Indicator

/** Scores an indicator from its per-fold backtests, withholding one search fold per generation.
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
    foldScores: Indicator => F[List[Double]],
    foldCount: Int,
    canonicalise: Indicator => Either[Throwable, Indicator] = indicator => Right(indicator),
    observer: Option[FoldRotatingEvaluator.Observer[F]] = None
) extends Evaluator[F, Indicator]:

  override def evaluateIndividual(indicator: Indicator, phase: EvaluationPhase): F[(Indicator, Fitness)] =
    evaluate(indicator, phase, (_, _) => MonadThrow[F].unit)

  /** A run-local view observes successful searches while sharing the existing fold-score cache and diagnostics. */
  def observingSearch(callback: (Indicator, Fitness) => F[Unit]): Evaluator[F, Indicator] =
    val onSearch = (candidate: Indicator, scores: List[Double]) =>
      callback(candidate, Fitness(IndicatorObjective.FoldAggregation.combine(scores)))
    new Evaluator[F, Indicator] {
      override def evaluateIndividual(indicator: Indicator, phase: EvaluationPhase): F[(Indicator, Fitness)] =
        phase match
          case EvaluationPhase.Search(_) => evaluate(indicator, phase, onSearch)
          case EvaluationPhase.Rescore   => FoldRotatingEvaluator.this.evaluateIndividual(indicator, phase)
    }

  private def evaluate(
      indicator: Indicator,
      phase: EvaluationPhase,
      onEvaluated: (Indicator, List[Double]) => F[Unit]
  ): F[(Indicator, Fitness)] =
    MonadThrow[F].fromEither(canonicalise(indicator)).flatMap { candidate =>
      observer.traverse_(_.requested(phase)).flatMap { _ =>
        foldScores(candidate)
          .flatTap(scores => observer.traverse_(_.observed(candidate, phase, scores)))
          .flatTap(scores => onEvaluated(candidate, scores))
          .map(scores => candidate -> Fitness(IndicatorObjective.FoldAggregation.combineExcluding(scores, withheldFold(phase))))
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
      observed: (Indicator, EvaluationPhase, List[Double]) => F[Unit],
      computationStarted: Indicator => F[Unit],
      computationCompleted: F[Unit],
      foldStarted: F[Unit],
      foldCompleted: F[Unit]
  )

  /** Cache only each fold's score: retaining OrderStats would keep every candidate's trades and equity curves for the entire search. Score
    * a fold before starting the next one so completed folds' histories can also be collected while a candidate is still running.
    */
  def cached[F[_]: Concurrent](
      backtests: List[Indicator => F[List[OrderStats]]],
      scoringFunction: ScoringFunction,
      canonicalise: Indicator => Either[Throwable, Indicator] = indicator => Right(indicator),
      observer: Option[Observer[F]] = None
  ): F[FoldRotatingEvaluator[F]] =
    cachedScores(backtests.map(run => indicator => run(indicator).map(scoringFunction.score)), canonicalise, observer)

  /** Accept already reduced fold computations so an objective can use compact baseline measurements without retaining candidate histories.
    * Initial scores are measurements prepared before search, with their actual work recorded by the caller.
    */
  private[optimizer] def cachedScores[F[_]: Concurrent](
      computations: List[Indicator => F[Double]],
      canonicalise: Indicator => Either[Throwable, Indicator],
      observer: Option[Observer[F]],
      initialScores: Map[Indicator, List[Double]] = Map.empty
  ): F[FoldRotatingEvaluator[F]] =
    memoize[F, Indicator, List[Double]] { indicator =>
      observer.traverse_(_.computationStarted(indicator)).flatMap { _ =>
        computations
          .traverse { compute =>
            observer.traverse_(_.foldStarted).flatMap { _ =>
              compute(indicator)
                .flatTap(_ => observer.traverse_(_.foldCompleted))
            }
          }
          .flatTap(_ => observer.traverse_(_.computationCompleted))
      }
    }.map { scores =>
      val cached = (indicator: Indicator) => initialScores.get(indicator).fold(scores(indicator))(Concurrent[F].pure)
      FoldRotatingEvaluator(cached, computations.size, canonicalise, observer)
    }
