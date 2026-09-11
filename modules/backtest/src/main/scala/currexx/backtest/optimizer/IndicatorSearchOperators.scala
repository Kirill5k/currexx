package currexx.backtest.optimizer

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.Population
import currexx.algorithms.operators.{Crossover, Initialiser, Mutator}
import currexx.domain.signal.Indicator

import scala.util.Random

/** Runs the numeric operators on searchable leaves only, then restores the full strategy before evaluation or reporting. Fixed inputs never
  * enter mutation, interpolation, random draws or bounds repair. Keeping them in the restored indicator also keeps their signal emission
  * and the original composite order intact.
  */
final case class IndicatorSearchOperators[F[_]](
    initialiser: Initialiser[F, Indicator],
    crossover: Crossover[F, Indicator],
    mutator: Mutator[F, Indicator]
)

object IndicatorSearchOperators {
  def make[F[_]](space: IndicatorSearchSpace, extraSeeds: List[Indicator] = Nil)(using
      F: Sync[F],
      r: Random
  ): F[IndicatorSearchOperators[F]] =
    for
      seeds       <- F.fromEither(space.projectSeeds(extraSeeds))
      initialiser <- IndicatorInitialiser.seeded[F](seeds)
      crossover   <- IndicatorCrossover.make[F]
      mutator     <- IndicatorMutator.make[F]
    yield IndicatorSearchOperators(
      new Initialiser[F, Indicator] {
        override def initialisePopulation(seed: Indicator, populationSize: Int, shuffle: Boolean): F[Population[Indicator]] =
          F.fromEither(space.project(seed)).flatMap {
            case Some(genes) =>
              initialiser
                .initialisePopulation(genes, populationSize, shuffle)
                .flatMap(_.traverse(g => F.fromEither(space.restore(Some(g)))))
            case None =>
              F.pure(Vector.fill(populationSize)(space.template))
          }
      },
      new Crossover[F, Indicator] {
        override def cross(par1: Indicator, par2: Indicator)(using r: Random): F[Indicator] =
          cross(par1, par2, 1.0)

        override def cross(par1: Indicator, par2: Indicator, crossoverProbability: Double)(using r: Random): F[Indicator] =
          F.fromEither {
            for
              first  <- space.project(par1)
              second <- space.project(par2)
            yield (first, second)
          }.flatMap {
            case (Some(first), Some(second)) =>
              crossover.cross(first, second, crossoverProbability)(using r).flatMap(g => F.fromEither(space.restore(Some(g))))
            case (None, None) => F.pure(space.template)
            case _            => F.raiseError(new IllegalArgumentException("Parents have different searchable structures"))
          }
      },
      new Mutator[F, Indicator] {
        override def mutate(ind: Indicator, mutationProbability: Double)(using r: Random): F[Indicator] =
          F.fromEither(space.project(ind)).flatMap {
            case Some(genes) => mutator.mutate(genes, mutationProbability)(using r).flatMap(g => F.fromEither(space.restore(Some(g))))
            case None        => F.pure(space.template)
          }
      }
    )
}
