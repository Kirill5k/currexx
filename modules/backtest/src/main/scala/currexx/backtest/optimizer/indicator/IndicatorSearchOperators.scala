package currexx.backtest.optimizer.indicator

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.Population
import currexx.algorithms.operators.{Crossover, Initialiser, Mutator}
import currexx.backtest.optimizer.{Checks, SeededPopulation}
import currexx.domain.signal.Indicator

import scala.util.Random

/** Apply numeric operators only to searchable leaves, then restore the original composite and fixed signal inputs. */
final case class IndicatorSearchOperators[F[_]](
    initialiser: Initialiser[F, Indicator],
    crossover: Crossover[F, Indicator],
    mutator: Mutator[F, Indicator]
)

object IndicatorSearchOperators {

  /** The complete-candidate search has already resolved and deduplicated these projected seeds. */
  private[optimizer] def fromProjectedSeeds[F[_]](space: IndicatorSearchSpace, seeds: List[Indicator])(using
      F: Sync[F],
      r: Random
  ): F[IndicatorSearchOperators[F]] =
    for
      initialiser <- IndicatorInitialiser.seeded[F](seeds)
      operators   <- variation[F](space)
    yield IndicatorSearchOperators(
      new Initialiser[F, Indicator] {
        override def initialisePopulation(seed: Indicator, populationSize: Int, shuffle: Boolean): F[Population[Indicator]] =
          F.fromEither(space.project(seed)).flatMap {
            case Some(genes) =>
              initialiser
                .initialisePopulation(genes, populationSize, shuffle)
                .flatMap(_.traverse(g => F.fromEither(space.restore(Some(g)))))
            case None =>
              SeededPopulation
                .build(Vector(space.template), populationSize, shuffle)((candidate, _) => F.pure(candidate), () => F.pure(space.template))
          }
      },
      operators.crossover,
      operators.mutator
    )

  final private[optimizer] case class Variation[F[_]](crossover: Crossover[F, Indicator], mutator: Mutator[F, Indicator])

  private[optimizer] def variation[F[_]](space: IndicatorSearchSpace)(using F: Sync[F]): F[Variation[F]] =
    for
      crossing <- IndicatorCrossover.make[F]
      mutation <- IndicatorMutator.make[F]
    yield Variation(
      new Crossover[F, Indicator] {
        private def projected(first: Indicator, second: Indicator)(cross: (Indicator, Indicator) => F[Indicator]): F[Indicator] =
          F.fromEither {
            for
              left  <- space.project(first)
              right <- space.project(second)
            yield (left, right)
          }.flatMap {
            case (Some(left), Some(right)) => cross(left, right).flatMap(result => F.fromEither(space.restore(Some(result))))
            case (None, None)              => F.pure(space.template)
            case _                         => F.raiseError(new IllegalArgumentException("Parents have different searchable structures"))
          }

        override def cross(first: Indicator, second: Indicator)(using Random): F[Indicator] =
          projected(first, second)(crossing.cross(_, _))
        override def cross(first: Indicator, second: Indicator, probability: Double)(using Random): F[Indicator] =
          projected(first, second)(crossing.cross(_, _, probability))
      },
      new Mutator[F, Indicator] {
        override def mutate(candidate: Indicator, probability: Double)(using Random): F[Indicator] =
          change(space, candidate)(mutation.mutate(_, probability))
      }
    )

  private[optimizer] def nearby[F[_]](space: IndicatorSearchSpace, candidate: Indicator, radius: Double)(using
      F: Sync[F],
      random: Random
  ): F[Indicator] =
    F.fromEither(Checks.scale(radius)).flatMap { _ =>
      IndicatorMutator.scaled[F](radius).flatMap(underlying => change(space, candidate)(underlying.mutate(_, 1.0)))
    }

  private[optimizer] def randomise[F[_]](space: IndicatorSearchSpace, candidate: Indicator)(using
      F: Sync[F],
      random: Random
  ): F[Indicator] = change(space, candidate)(IndicatorInitialiser.randomise[F])

  private def change[F[_]](space: IndicatorSearchSpace, candidate: Indicator)(operation: Indicator => F[Indicator])(using
      F: Sync[F]
  ): F[Indicator] =
    F.fromEither(space.project(candidate)).flatMap {
      case Some(genes) => operation(genes).flatMap(result => F.fromEither(space.restore(Some(result))))
      case None        => F.pure(space.template)
    }
}
