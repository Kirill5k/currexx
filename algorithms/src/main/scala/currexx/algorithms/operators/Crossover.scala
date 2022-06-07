package currexx.algorithms.operators

import currexx.algorithms.Ind

import scala.util.Random

trait Crossover[A]:
  def cross(par1: Ind[A], par2: Ind[A], crossoverProbability: Double)(using r: Random): Option[Ind[A]]