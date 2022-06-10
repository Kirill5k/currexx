package currexx.algorithms.operators

import currexx.algorithms.Population

trait Initialiser[I]:
  def initialisePopulation(seed: I, populationSize: Int, shuffle: Boolean): Population[I]
