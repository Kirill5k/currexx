package currexx.backtest.optimizer

import cats.effect.{Concurrent, Ref}
import cats.syntax.functor.*
import currexx.algorithms.{EvaluatedPopulation, Fitness}
import currexx.domain.signal.Indicator

/** Bounded search evidence, independent of the evolving population and diagnostic reporting.
  *
  * Callers supply canonical indicators and all-fold fitness. Repeated observations retain the best score for the same candidate.
  */
final class SearchArchive[F[_]] private (
    capacity: Int,
    state: Ref[F, EvaluatedPopulation[Indicator]]
) {
  def record(indicator: Indicator, fullSearchFitness: Fitness): F[Unit] =
    state.update { current =>
      val candidate = indicator -> fullSearchFitness
      if (current.size >= capacity && SearchArchive.candidateOrder.gteq(candidate, current.last)) current
      else if (current.exists { case (existing, previous) => existing == indicator && previous.value >= fullSearchFitness.value }) current
      else
        (current.filterNot(_._1 == indicator) :+ candidate)
          .sorted(using SearchArchive.candidateOrder)
          .take(capacity)
    }

  def candidates: F[EvaluatedPopulation[Indicator]] = state.get
}

object SearchArchive {
  private val candidateOrder: Ordering[(Indicator, Fitness)] =
    Ordering.by[(Indicator, Fitness), Double](-_._2.value).orElseBy(_._1.toString)

  def make[F[_]: Concurrent](capacity: Int): F[SearchArchive[F]] =
    if (capacity <= 0) Concurrent[F].raiseError(new IllegalArgumentException("Search archive capacity must be positive"))
    else Ref.of[F, EvaluatedPopulation[Indicator]](Vector.empty).map(new SearchArchive(capacity, _))
}
