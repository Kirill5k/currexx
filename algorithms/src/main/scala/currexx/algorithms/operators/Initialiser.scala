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
