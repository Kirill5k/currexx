package currexx.algorithms.operators

import cats.Id
import cats.effect.Sync

import scala.util.Random

trait Mutator[F[_], I]:
  def mutate(ind: I, mutationProbability: Double)(using r: Random): F[I]

object Mutator:
  def pureBitFlip: Mutator[Id, Array[Int]] = new Mutator[Id, Array[Int]]:
    override def mutate(ind: Array[Int], mutationProbability: Double)(using r: Random): Id[Array[Int]] =
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

  def bitFlip[F[_]](using F: Sync[F]): F[Mutator[F, Array[Int]]] =
    F.pure {
      new Mutator[F, Array[Int]] {
        val mutator = pureBitFlip
        override def mutate(ind: Array[Int], mutationProbability: Double)(using r: Random): F[Array[Int]] =
          F.delay(mutator.mutate(ind, mutationProbability))
      }
    }

  def neighbourSwap[F[_], G](using F: Sync[F]): F[Mutator[F, Array[G]]] =
    F.pure {
      new Mutator[F, Array[G]] {
        override def mutate(ind: Array[G], mutationFactor: Double)(using r: Random): F[Array[G]] =
          F.delay {
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
