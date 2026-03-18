package currexx.algorithms.progress

import cats.Monad
import currexx.algorithms.{EvaluatedPopulation, Parameters}

final class NoopTracker[F[_], I](using
    F: Monad[F]
) extends Tracker[F, I] {
  
  override def displayInitial(target: I, params: Parameters.GA): F[Unit]                                  = F.unit
  override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit] = F.unit
  override def displayFinal(population: EvaluatedPopulation[I]): F[Unit]                                  = F.unit
}
