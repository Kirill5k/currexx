package currexx.algorithms.operators

import scala.util.Random

trait Mutator[I]:
  def mutate(ind: I, mutationProbability: Double)(using r: Random): I

object Mutator:
  inline def neighbourSwap[G]: Mutator[Array[G]] = new Mutator[Array[G]] {
    override def mutate(ind: Array[G], mutationFactor: Double)(using r: Random): Array[G] = {
      val result = ind.clone()
      var i      = 0
      while (i < result.length - 1) {
        if (r.nextDouble() < mutationFactor) {
          val curr = result(i)
          val next = result(i + 1)
          result(i) = next
          result(i + 1) = curr
        }
        i += 1
      }

      result
    }
  }