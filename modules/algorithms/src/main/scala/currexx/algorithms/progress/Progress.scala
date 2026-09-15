package currexx.algorithms.progress

import currexx.algorithms.EvaluatedPopulation
import currexx.algorithms.operators.species.SpeciesStats

sealed trait Progress[I]:
  def currentGen: Int
  def maxGen: Int
  def population: EvaluatedPopulation[I]

object Progress:
  final case class Population[I](currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]) extends Progress[I]

  /** The population has completed this generation; species statistics describe the parents used to breed it. */
  final case class Species[I](
      currentGen: Int,
      maxGen: Int,
      population: EvaluatedPopulation[I],
      breedingSpecies: SpeciesStats
  ) extends Progress[I]
