package currexx.algorithms.operators

import cats.effect.Sync
import currexx.algorithms.{EvaluatedPopulation, Fitness, Population}

trait Elitism[F[_], I]:
  def select(population: EvaluatedPopulation[I], n: Double): F[Population[I]]

object Elitism:
  inline def simple[F[_]: Sync, A]: Elitism[F, A] = new Elitism[F, A] {
    override def select(population: EvaluatedPopulation[A], n: Double): F[Population[A]] =
      Sync[F].delay(population.sortBy(_._2)(Ordering[Fitness].reverse).take(n.toInt).map(_._1))
  }
