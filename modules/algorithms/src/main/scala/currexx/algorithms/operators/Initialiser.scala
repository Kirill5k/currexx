package currexx.algorithms.operators

import cats.effect.Sync
import currexx.algorithms.Population
import fs2.Stream

trait Initialiser[F[_], I]:
  def initialisePopulation(seed: I, populationSize: Int, shuffle: Boolean): F[Population[I]]

object Initialiser:
  def simple[F[_], I](randomise: I => F[I])(using F: Sync[F]): F[Initialiser[F, I]] =
    F.pure {
      new Initialiser[F, I]:
        override def initialisePopulation(seed: I, populationSize: Int, shuffle: Boolean): F[Population[I]] =
          if (shuffle) Stream.eval(randomise(seed)).repeatN(populationSize).compile.toVector
          else F.delay(Vector.fill(populationSize)(seed))
    }

  /** Builds the whole population at once rather than one member at a time.
    *
    * `simple` can only apply the same draw to every member, which makes a shuffled population uniformly random and leaves no way to mix
    * members of different kinds - clones of the seed, near neighbours of it, and independent draws - in chosen proportions. That mixture is
    * the difference between a population with a quality floor and one without, so an initialiser that wants it needs the size and the seed
    * together.
    */
  def custom[F[_], I](build: (I, Int, Boolean) => F[Population[I]])(using F: Sync[F]): F[Initialiser[F, I]] =
    F.pure {
      new Initialiser[F, I]:
        override def initialisePopulation(seed: I, populationSize: Int, shuffle: Boolean): F[Population[I]] =
          build(seed, populationSize, shuffle)
    }
