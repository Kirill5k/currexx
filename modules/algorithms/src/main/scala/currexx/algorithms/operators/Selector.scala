package currexx.algorithms.operators

import cats.Id
import cats.effect.Sync
import currexx.algorithms.{DistributedPopulation, EvaluatedPopulation, Fitness}
import currexx.algorithms.collections.*

import scala.collection.mutable.ListBuffer
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
          var cumulative    = 0.0
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

  /** Pure binary tournament selection WITH replacement.
    *
    * Each slot is filled by drawing two distinct individuals uniformly at random and keeping the fitter one. Winners stay in the pool, so a
    * strong individual can win several tournaments and contribute its genes to several children — that amplification is the entire source
    * of selection pressure.
    *
    * Drawing without replacement would remove it: callers ask for `populationLimit == populationSize` selections because each pair yields
    * two children, so every individual would become a parent exactly once and fitness would only decide which handful of individuals are
    * left over, leaving the search close to a random walk.
    *
    * The algorithm:
    *   1. For each selection, randomly pick two different individuals from the population
    *   2. Compare their fitness values and select the fitter individual
    *   3. Repeat until `populationLimit` individuals have been selected, then group them into pairs
    */
  def pureTournament[I] = new Selector[Id, I] {
    override def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): Id[DistributedPopulation[I]] =
      if (population.isEmpty) Vector.empty
      else {
        val selectedIndividuals = ListBuffer.empty[I]
        while (selectedIndividuals.size < populationLimit)
          selectedIndividuals += selectOne(population)
        selectedIndividuals.toVector.pairs
      }

    private def selectOne(population: EvaluatedPopulation[I])(using r: Random): I =
      if (population.length == 1) population.head._1
      else {
        val p1Index = r.nextInt(population.length)
        var p2Index = r.nextInt(population.length)
        // Ensure we don't select the same individual twice within the tournament
        while (p2Index == p1Index)
          p2Index = r.nextInt(population.length)
        val p1 = population(p1Index)
        val p2 = population(p2Index)
        if (p1._2 > p2._2) p1._1 else p2._1
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
