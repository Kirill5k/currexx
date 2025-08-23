package currexx.algorithms.operators

import cats.Id
import cats.effect.Sync
import currexx.algorithms.{DistributedPopulation, EvaluatedPopulation, Fitness}
import currexx.algorithms.collections.*

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

trait Selector[F[_], I]:
  def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): F[DistributedPopulation[I]]

object Selector:

  /** Pure roulette wheel selection implementation.
    *
    * This method implements fitness-proportionate selection where individuals with higher fitness have a higher probability of being
    * selected. The selection process uses "selection with replacement", meaning the same individual can be selected multiple times, which
    * is standard behavior for genetic algorithms.
    *
    * The algorithm:
    *   1. Calculates the total fitness sum of all individuals
    *   2. For each selection, generates a random number between 0 and fitness sum
    *   3. Iterates through the population, accumulating fitness values until the random threshold is reached
    *   4. Selects the individual at that position
    *
    * Special handling for zero fitness: If all fitness values are zero, selection becomes random to avoid division by zero and ensure the
    * algorithm can still proceed.
    */
  def pureRouletteWheel[I] = new Selector[Id, I] {
    override def selectPairs(popByFitness: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): Id[DistributedPopulation[I]] = {
      val newPop     = ListBuffer.empty[I]
      val fitnessSum = popByFitness.map(_._2).sum

      while (newPop.size < populationLimit && popByFitness.nonEmpty)
        if (fitnessSum.isZero) {
          // If all fitness values are zero, select randomly
          val individual = popByFitness(r.nextInt(popByFitness.length))._1
          newPop.addOne(individual)
        } else {
          val random        = r.nextDouble() * fitnessSum.value
          var cumulative    = BigDecimal(0.0)
          var selectedIndex = -1
          val it            = popByFitness.iterator.zipWithIndex
          while (it.hasNext && selectedIndex == -1) {
            val ((_, fitness), i) = it.next()
            cumulative += fitness.value
            if (cumulative >= random) {
              selectedIndex = i
            }
          }
          val finalIndex = if (selectedIndex == -1) popByFitness.length - 1 else selectedIndex
          val individual = popByFitness(finalIndex)._1
          newPop.addOne(individual)
        }
      newPop.toVector.pairs
    }
  }

  def rouletteWheel[F[_], I](using F: Sync[F]): F[Selector[F, I]] =
    F.pure {
      new Selector[F, I] {
        val rouletteWheelSelector = pureRouletteWheel[I]
        override def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): F[DistributedPopulation[I]] =
          F.delay(rouletteWheelSelector.selectPairs(population, populationLimit))
      }
    }

  /** Pure tournament selection implementation with selection WITHOUT replacement.
    *
    * This method implements binary tournament selection where two random individuals compete in each tournament, and the one with higher
    * fitness wins. Unlike standard tournament selection, this implementation uses "selection without replacement" at the population level,
    * meaning once an individual is selected, it cannot be selected again until all selections are complete.
    *
    * The algorithm:
    *   1. Create a mutable copy of the population for tracking remaining individuals
    *   2. For each selection, randomly pick two different individuals from remaining population
    *   3. Compare their fitness values and select the fitter individual
    *   4. Remove the selected individual from the remaining population
    *   5. Ensure no individual competes against itself within a tournament
    *
    * This approach ensures maximum diversity in selection but may reduce selection pressure compared to selection with replacement.
    */
  def pureTournament[I] = new Selector[Id, I] {
    override def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): Id[DistributedPopulation[I]] = {
      val selectedIndividuals = ListBuffer.empty[I]
      val remainingPopulation = ArrayBuffer.from(population)

      while (selectedIndividuals.size < populationLimit && remainingPopulation.nonEmpty) {
        val (selectedIndividual, indexToRemove) = selectOneWithIndex(remainingPopulation)
        selectedIndividuals += selectedIndividual

        // Remove the selected individual efficiently using swap-remove
        if (indexToRemove >= 0) {
          // Swap with last element and remove last (O(1) operation)
          if (indexToRemove < remainingPopulation.length - 1) {
            remainingPopulation(indexToRemove) = remainingPopulation.last
          }
          val _ = remainingPopulation.remove(remainingPopulation.length - 1)
        }
      }

      selectedIndividuals.toVector.pairs
    }

    private def selectOneWithIndex(population: ArrayBuffer[(I, Fitness)])(using r: Random): (I, Int) =
      if (population.length == 1) {
        (population.head._1, 0)
      } else {
        val p1Index = r.nextInt(population.length)
        var p2Index = r.nextInt(population.length)
        // Ensure we don't select the same individual twice within the tournament
        while (p2Index == p1Index)
          p2Index = r.nextInt(population.length)
        val p1 = population(p1Index)
        val p2 = population(p2Index)
        if (p1._2 > p2._2) (p1._1, p1Index) else (p2._1, p2Index)
      }
  }

  def tournament[F[_], I](using F: Sync[F]): F[Selector[F, I]] =
    F.pure {
      new Selector[F, I] {
        val tournamentSelector = pureTournament[I]
        override def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): F[DistributedPopulation[I]] =
          F.delay(tournamentSelector.selectPairs(population, populationLimit))
      }
    }
