package currexx.algorithms.operators

import cats.effect.Sync

import scala.util.Random

trait Mutator[F[_], I]:
  def mutate(ind: I, mutationProbability: Double)(using r: Random): F[I]

object Mutator:
  inline def neighbourSwap[F[_]: Sync, G]: Mutator[F, Array[G]] = new Mutator[F, Array[G]] {
    override def mutate(ind: Array[G], mutationFactor: Double)(using r: Random): F[Array[G]] =
      Sync[F].delay {
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