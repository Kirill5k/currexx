package currexx.algorithms.operators.species

import currexx.algorithms.Population

/** An evaluated population grouped into species, before or after conservation. */
final case class SpeciesPopulation[I](species: Vector[Species[I]]):
  def sizes: Vector[Int]             = species.map(_.members.size)
  def population: Population[I]      = species.flatMap(_.members.map(_._1))
  def representatives: Population[I] = species.map(_.representative._1)
  def distinctCandidates: Int        = population.distinct.size
