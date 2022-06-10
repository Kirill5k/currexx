package currexx.algorithms.operators

import scala.util.Random

trait Mutator[I]:
  def mutate(ind: I, mutationProbability: Double)(using r: Random): I
