package currexx.algorithms.operators.species

import cats.effect.Sync
import currexx.algorithms.EvaluatedPopulation

import scala.util.Random

trait SpeciesSelection[F[_], I]:
  def select(
      population: SpeciesPopulation[I],
      populationSize: Int,
      interspeciesProbability: Double
  )(using
      Random
  ): F[Either[IllegalArgumentException, SpeciesBreeding[I]]]

object SpeciesSelection:
  private def errorOnCond(test: Boolean, message: String): Either[IllegalArgumentException, Unit] =
    Either.cond(test, (), new IllegalArgumentException(message))

  def make[F[_], I](using F: Sync[F]): F[SpeciesSelection[F, I]] =
    F.pure(new SpeciesSelection[F, I] {
      override def select(
          population: SpeciesPopulation[I],
          populationSize: Int,
          interspeciesProbability: Double
      )(using
          random: Random
      ): F[Either[IllegalArgumentException, SpeciesBreeding[I]]] =
        F.delay {
          for
            species = population.species
            _ <- errorOnCond(populationSize > 0, "Population size must be positive")
            _ <- errorOnCond(species.nonEmpty && species.forall(_.members.nonEmpty), "Breeding requires nonempty species")
            _ <- errorOnCond(
              interspeciesProbability.isFinite && interspeciesProbability >= 0.0 && interspeciesProbability <= 1.0,
              "Interspecies probability must be between zero and one"
            )
            _ <- errorOnCond(
              (populationSize == 1 && species.size == 1) || species.size <= populationSize / 2,
              "Population must fit a representative and a child per species"
            )
          yield {
            val counts = offspringCounts(species, populationSize)
            val pairs  = species.indices.toVector.flatMap { index =>
              Vector.fill(counts(index)) {
                val first         = tournament(species(index).members)
                val secondSpecies =
                  if (species.size > 1 && interspeciesProbability > 0.0 && random.nextDouble() < interspeciesProbability) {
                    val other = random.nextInt(species.size - 1)
                    if (other >= index) other + 1 else other
                  } else index
                (first, tournament(species(secondSpecies).members))
              }
            }
            SpeciesBreeding(pairs, counts)
          }
        }
    })

  private def offspringCounts[I](species: Vector[Species[I]], populationSize: Int): Vector[Int] =
    if (populationSize == 1) Vector(0)
    else {
      val count     = species.size
      val remaining = populationSize - 2 * count
      // Ranks start at one. Averaging occupied ranks gives equal champions equal allocation weight,
      // without assuming positive fitness or changing the objective's scores.
      val weights = species.map { group =>
        val score  = group.representative._2
        val better = species.count(_.representative._2 > score)
        val tied   = species.count(_.representative._2 == score)
        count + 1.0 - (better + 1.0 + (tied - 1) / 2.0)
      }
      val totalWeight = weights.sum
      val shares      = weights.map(weight => remaining.toDouble * weight / totalWeight)
      val whole       = shares.map(_.toInt)
      val residual    = remaining - whole.sum
      val extra       = shares.indices.sortBy(index => (-(shares(index) - whole(index)), index)).take(residual).toSet
      whole.indices.toVector.map(index => 1 + whole(index) + (if (extra.contains(index)) 1 else 0))
    }

  private def tournament[I](population: EvaluatedPopulation[I])(using random: Random): I =
    if (population.size == 1) population.head._1
    else {
      val first  = random.nextInt(population.size)
      val draw   = random.nextInt(population.size - 1)
      val second = if (draw >= first) draw + 1 else draw
      if (population(first)._2 > population(second)._2) population(first)._1 else population(second)._1
    }
