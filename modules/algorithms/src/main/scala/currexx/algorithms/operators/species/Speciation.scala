package currexx.algorithms.operators.species

import cats.effect.Sync
import cats.syntax.traverse.*
import currexx.algorithms.{EvaluatedPopulation, Fitness}

trait Speciation[F[_], I]:
  def partition(
      population: EvaluatedPopulation[I],
      radius: Double,
      maxSpecies: Int
  ): F[Either[IllegalArgumentException, SpeciesPopulation[I]]]

object Speciation:
  def make[F[_], I](distance: Distance[I])(using F: Sync[F]): F[Speciation[F, I]] =
    F.pure(new Speciation[F, I] {
      override def partition(
          population: EvaluatedPopulation[I],
          radius: Double,
          maxSpecies: Int
      ): F[Either[IllegalArgumentException, SpeciesPopulation[I]]] =
        F.delay {
          for
            _ <- Either.cond(
              radius.isFinite && radius >= 0.0,
              (),
              new IllegalArgumentException("Species radius must be finite and nonnegative")
            )
            _           <- Either.cond(maxSpecies > 0, (), new IllegalArgumentException("Maximum species count must be positive"))
            partitioned <- population
              .sortBy(_._2)(using Ordering[Fitness].reverse)
              .foldLeft[Either[IllegalArgumentException, Vector[Species[I]]]](Right(Vector.empty)) { (result, member) =>
                result.flatMap { species =>
                  if (species.isEmpty) Right(Vector(Species(member, Vector(member))))
                  else {
                    species
                      .traverse { group =>
                        distance.between(member._1, group.representative._1).flatMap { value =>
                          Either.cond(
                            value.isFinite && value >= 0.0,
                            value,
                            new IllegalArgumentException("Species distance must be finite and nonnegative")
                          )
                        }
                      }
                      .map { distances =>
                        // The first minimum wins, so both distance and fitness ties retain creation order.
                        val nearest = distances.indices.minBy(distances)
                        if (distances(nearest) <= radius || species.size >= maxSpecies) {
                          val group = species(nearest)
                          species.updated(nearest, group.copy(members = group.members :+ member))
                        } else species :+ Species(member, Vector(member))
                      }
                  }
                }
              }
          yield SpeciesPopulation(partitioned)
        }
    })
