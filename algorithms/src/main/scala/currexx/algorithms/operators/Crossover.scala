package currexx.algorithms.operators

import scala.util.Random

trait Crossover[I]:
  def cross(par1: I, par2: I, crossoverProbability: Double)(using r: Random): Option[I]