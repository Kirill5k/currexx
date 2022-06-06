package currexx.algorithms.operators

import currexx.algorithms.{EvaluatedPopulation, Population}

trait Elitism[A]:
  def select(population: EvaluatedPopulation[A], n: Double): Population[A]
