package currexx.backtest.optimizer

import cats.effect.{Concurrent, Ref}
import cats.syntax.functor.*
import currexx.algorithms.{EvaluatedPopulation, Fitness}
import currexx.backtest.TestStrategy

/** Bounded search evidence, independent of the evolving population and diagnostic reporting.
  *
  * Callers supply canonical candidates and all-fold fitness. Repeated observations retain the best score for the same candidate.
  */
final class SearchArchive[F[_]] private (
    capacity: Int,
    state: Ref[F, EvaluatedPopulation[TestStrategy]],
    candidateOrder: Ordering[(TestStrategy, Fitness)]
) {
  def record(candidate: TestStrategy, fullSearchFitness: Fitness): F[Unit] =
    state.update { current =>
      val member = candidate -> fullSearchFitness
      if (current.size >= capacity && candidateOrder.gteq(member, current.last)) current
      else if (current.exists { case (existing, previous) => existing == candidate && previous.value >= fullSearchFitness.value }) current
      else
        (current.filterNot(_._1 == candidate) :+ member)
          .sorted(using candidateOrder)
          .take(capacity)
    }

  def candidates: F[EvaluatedPopulation[TestStrategy]] = state.get
}

object SearchArchive {
  private val candidateOrder: Ordering[(TestStrategy, Fitness)] =
    Ordering.by[(TestStrategy, Fitness), Double](-_._2.value).orElseBy(_._1.toString)

  def make[F[_]: Concurrent](capacity: Int): F[SearchArchive[F]] =
    if (capacity <= 0) Concurrent[F].raiseError(new IllegalArgumentException("Search archive capacity must be positive"))
    else Ref.of[F, EvaluatedPopulation[TestStrategy]](Vector.empty).map(new SearchArchive(capacity, _, candidateOrder))
}
