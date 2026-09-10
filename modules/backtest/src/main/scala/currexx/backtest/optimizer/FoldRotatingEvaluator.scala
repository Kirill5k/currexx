package currexx.backtest.optimizer

import cats.Functor
import cats.effect.Concurrent
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
final class FoldRotatingEvaluator[F[_]: Functor](
    foldScores: Indicator => F[List[Double]],
    foldCount: Int
) extends Evaluator[F, Indicator]:

  override def evaluateIndividual(indicator: Indicator, phase: EvaluationPhase): F[(Indicator, Fitness)] =
    foldScores(indicator).map { scores =>
      indicator -> Fitness(IndicatorObjective.FoldAggregation.combineExcluding(scores, withheldFold(phase)))
    }

  /** Which fold this phase does not get to count, if any. */
  private[optimizer] def withheldFold(phase: EvaluationPhase): Option[Int] = phase match
    case EvaluationPhase.Rescore => None
    // Withholding the only fold there is would score every candidate on nothing and rank them all equal, so a corpus with none to spare
    // is read whole. Two is the fewest that can lose one and still say something.
    case EvaluationPhase.Search(_) if foldCount <= 1 => None
    case EvaluationPhase.Search(generation)          => Some(math.floorMod(generation, foldCount))

object FoldRotatingEvaluator:

  /** Cache only each fold's score: retaining OrderStats would keep every candidate's trades and equity curves for the entire search.
    * Score a fold before starting the next one so completed folds' histories can also be collected while a candidate is still running.
    */
  def cached[F[_]: Concurrent](
      backtests: List[Indicator => F[List[OrderStats]]],
      scoringFunction: ScoringFunction
  ): F[FoldRotatingEvaluator[F]] =
    memoize[F, Indicator, List[Double]] { indicator =>
      backtests.traverse(backtest => backtest(indicator).map(scoringFunction.score))
    }.map(scores => FoldRotatingEvaluator(scores, backtests.size))
