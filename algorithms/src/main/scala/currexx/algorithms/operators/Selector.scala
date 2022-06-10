package currexx.algorithms.operators

import currexx.algorithms.{DistributedPopulation, EvaluatedPopulation}

import scala.util.Random

trait Selector[I]:
  def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): DistributedPopulation[I]

