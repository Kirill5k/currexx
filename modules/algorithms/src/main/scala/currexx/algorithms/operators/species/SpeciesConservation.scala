package currexx.algorithms.operators.species

import cats.effect.Sync
import currexx.algorithms.Fitness

trait SpeciesConservation[F[_], I]:
  def conserve(population: SpeciesPopulation[I], size: Int): F[Either[IllegalArgumentException, SpeciesPopulation[I]]]

object SpeciesConservation:
  def make[F[_], I](using F: Sync[F]): F[SpeciesConservation[F, I]] =
    F.pure(new SpeciesConservation[F, I] {
      override def conserve(
          population: SpeciesPopulation[I],
          size: Int
      ): F[Either[IllegalArgumentException, SpeciesPopulation[I]]] =
        F.delay {
          val species = population.species
          for
            _ <- Either.cond(size >= 0, (), new IllegalArgumentException("Conserved population size must be nonnegative"))
            _ <- Either.cond(
              species.forall(group => group.members.contains(group.representative)),
              (),
              new IllegalArgumentException("Every species representative must be one of its members")
            )
          yield {
            // Tags identify occurrences, including identical candidates within or across groups.
            val occurrences = species.zipWithIndex.flatMap { case (group, groupIndex) =>
              group.members.zipWithIndex.map { case (member, memberIndex) => (groupIndex, memberIndex, member) }
            }
            val reserved = species.zipWithIndex
              .sortBy(_._1.representative._2)(using Ordering[Fitness].reverse)
              .take(size)
              .map { case (group, groupIndex) => (groupIndex, group.members.indexOf(group.representative)) }
              .toSet
            val fill = occurrences
              .filterNot { case (groupIndex, memberIndex, _) => reserved.contains((groupIndex, memberIndex)) }
              .sortBy(_._3._2)(using Ordering[Fitness].reverse)
              .take(size - reserved.size)
              .map { case (groupIndex, memberIndex, _) => (groupIndex, memberIndex) }
            val selected = reserved ++ fill

            val conserved = species.zipWithIndex.flatMap { case (group, groupIndex) =>
              val retained = group.members.zipWithIndex
                .collect { case (member, memberIndex) if selected.contains((groupIndex, memberIndex)) => member }
                .sortBy(_._2)(using Ordering[Fitness].reverse)
              retained.headOption.map(representative => Species(representative, retained))
            }
            SpeciesPopulation(conserved)
          }
        }
    })
