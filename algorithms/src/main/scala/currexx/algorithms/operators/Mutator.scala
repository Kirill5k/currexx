package currexx.algorithms.operators

import currexx.algorithms.Ind

import scala.util.Random

trait Mutator[A]:
  def mutate(ind: Ind[A], mutationProbability: Double)(using r: Random): Ind[A]
