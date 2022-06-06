package currexx.algorithms.operators

import currexx.algorithms.{DistributedPopulation, EvaluatedPopulation}

import scala.util.Random

trait Selector[A]:
  def selectPairs(population: EvaluatedPopulation[A], populationLimit: Int)(using r: Random): DistributedPopulation[A]

