package currexx.algorithms.operators

import currexx.algorithms.{EvaluatedPopulation, Fitness, Population}

trait Elitism[I]:
  def select(population: EvaluatedPopulation[I], n: Double): Population[I]

object Elitism:
  inline def simple[A]: Elitism[A] = new Elitism[A] {
    override def select(population: EvaluatedPopulation[A], n: Double): Population[A] =
      population.sortBy(_._2)(Ordering[Fitness].reverse).take(n.toInt).map(_._1)
  }