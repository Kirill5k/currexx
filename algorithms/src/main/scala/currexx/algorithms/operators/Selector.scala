package currexx.algorithms.operators

import cats.Id
import cats.effect.Sync
import currexx.algorithms.{DistributedPopulation, EvaluatedPopulation, Fitness, Population}
import currexx.algorithms.collections.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

trait Selector[F[_], I]:
  def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): F[DistributedPopulation[I]]

object Selector:
  def pureRouletteWheel[I] = new Selector[Id, I] {
    override def selectPairs(popByFitness: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): Id[DistributedPopulation[I]] = {
      val newPop     = ListBuffer.empty[I]
      var remPop     = popByFitness
      var remFitness = popByFitness.map(_._2).reduce(_ + _)
      while (remPop.nonEmpty && newPop.size < populationLimit) {
        val ((pickedInd, indFitness), remainingIndividuals) = pickOne(remPop, remFitness)
        newPop.addOne(pickedInd)
        remPop = remainingIndividuals
        remFitness = remFitness - indFitness
      }
      newPop.toVector.pairs
    }

    private def pickOne(
        popByFitness: EvaluatedPopulation[I],
        fTotal: Fitness
    )(using r: Random): ((I, Fitness), EvaluatedPopulation[I]) = {
      var remFitness = BigDecimal(1.0)

      val chance = r.nextDouble()
      val indIndex = LazyList
        .from(popByFitness)
        .map { case (i, f) =>
          val res   = (i, remFitness)
          val ratio = if (fTotal.isZero) f else f / fTotal
          remFitness = remFitness - ratio.value
          res
        }
        .indexWhere(_._2 < chance, 0) - 1

      if (indIndex >= 0) {
        val ind    = popByFitness(indIndex)
        val remPop = popByFitness.take(indIndex) ++ popByFitness.drop(indIndex + 1)
        (ind, remPop)
      } else {
        (popByFitness.head, popByFitness.tail)
      }
    }
  }

  def rouletteWheel[F[_]: Sync, I](using F: Sync[F]): F[Selector[F, I]] =
    F.pure {
      new Selector[F, I] {
        val rouletteWheelSelector = pureRouletteWheel[I]
        override def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): F[DistributedPopulation[I]] =
          F.delay(rouletteWheelSelector.selectPairs(population, populationLimit))
      }
    }
