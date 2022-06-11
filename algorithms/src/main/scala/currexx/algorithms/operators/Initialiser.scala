package currexx.algorithms.operators

import cats.effect.Sync
import currexx.algorithms.Population
import fs2.Stream

trait Initialiser[F[_], I]:
  def initialisePopulation(seed: I, populationSize: Int, shuffle: Boolean): F[Population[I]]

object Initialiser:
  def simple[F[_]: Sync, I](randomise: I => F[I]): F[Initialiser[F, I]] =
    Sync[F].delay {
      new Initialiser[F, I] {
        override def initialisePopulation(seed: I, populationSize: Int, shuffle: Boolean): F[Population[I]] =
          if (shuffle) Stream.eval(randomise(seed)).repeatN(populationSize).compile.toVector
          else Sync[F].delay(Vector.fill(populationSize)(seed))
      }
    }
