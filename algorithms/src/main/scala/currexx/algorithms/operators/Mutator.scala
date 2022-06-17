package currexx.algorithms.operators

import cats.effect.Sync

import scala.util.Random

trait Mutator[F[_], I]:
  def mutate(ind: I, mutationProbability: Double)(using r: Random): F[I]

object Mutator:
  def bitFlip[F[_]: Sync]: F[Mutator[F, Array[Int]]] =
    Sync[F].pure {
      new Mutator[F, Array[Int]] {
        override def mutate(ind: Array[Int], mutationProbability: Double)(using r: Random): F[Array[Int]] =
          Sync[F].delay {
            val result = ind.clone()
            var i      = 0
            while (i < result.length - 1) {
              if (r.nextDouble() < mutationProbability) {
                val curr = result(i)
                result(i) = if (curr == 0) 1 else 0
              }
              i += 1
            }
            result
          }
      }
    }

  def neighbourSwap[F[_]: Sync, G]: F[Mutator[F, Array[G]]] =
    Sync[F].pure {
      new Mutator[F, Array[G]] {
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
    }
