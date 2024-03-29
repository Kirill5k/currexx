package currexx.algorithms.operators

import cats.effect.Sync
import currexx.algorithms.{EvaluatedPopulation, Population}

trait Elitism[F[_], I]:
  def select(population: EvaluatedPopulation[I], n: Double): F[Population[I]]

object Elitism:
  def simple[F[_], A](using F: Sync[F]): F[Elitism[F, A]] =
    F.pure {
      new Elitism[F, A]:
        override def select(sortedPopulation: EvaluatedPopulation[A], n: Double): F[Population[A]] =
          F.delay(sortedPopulation.take(n.toInt).map(_._1))
    }
