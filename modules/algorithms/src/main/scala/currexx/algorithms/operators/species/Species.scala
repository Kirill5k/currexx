package currexx.algorithms.operators.species

import cats.effect.Sync
import cats.syntax.all.*
import currexx.algorithms.{DistributedPopulation, EvaluatedPopulation, Fitness}

/** Distance in the caller's search space. Equal candidates have zero distance; incompatible inputs or invalid distances return Left. */
trait Distance[I]:
  def between(a: I, b: I): Either[IllegalArgumentException, Double]

final case class Species[I](representative: (I, Fitness), members: EvaluatedPopulation[I])

/** One ordered parent pair per child, grouped in species order. Counts exclude the protected representatives. */
final case class SpeciesBreeding[I](pairs: DistributedPopulation[I], offspringCounts: Vector[Int])

/** Sizes and distinct candidates describe the parent population used for breeding; counts describe its allocated children. */
final case class SpeciesStats(sizes: Vector[Int], offspringCounts: Vector[Int], distinctCandidates: Int)

final case class SpeciesOperators[F[_], I](
    speciation: Speciation[F, I],
    selection: SpeciesSelection[F, I],
    conservation: SpeciesConservation[F, I]
)

object SpeciesOperators:
  def make[F[_]: Sync, I](distance: Distance[I]): F[SpeciesOperators[F, I]] =
    for
      speciation   <- Speciation.make[F, I](distance)
      selection    <- SpeciesSelection.make[F, I]
      conservation <- SpeciesConservation.make[F, I]
    yield SpeciesOperators(speciation, selection, conservation)
