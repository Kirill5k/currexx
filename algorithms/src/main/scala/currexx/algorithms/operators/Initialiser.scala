package currexx.algorithms.operators

import currexx.algorithms.Population

trait Initialiser[F[_], I]:
  def initialisePopulation(seed: I, populationSize: Int, shuffle: Boolean): F[Population[I]]