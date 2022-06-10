package currexx.algorithms.operators

import currexx.algorithms.{EvaluatedPopulation, Population}

trait Elitism[I]:
  def select(population: EvaluatedPopulation[I], n: Double): Population[I]
