package currexx.backtest.optimizer

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.Population

/** Preserve each distinct seed before allocating nearby candidates and random immigrants. */
private[optimizer] object SeededPopulation {
  def build[F[_], A](seeds: Vector[A], size: Int, shuffle: Boolean)(
      jitter: (A, Double) => F[A],
      randomise: () => F[A]
  )(using F: Sync[F]): F[Population[A]] = F.defer {
    val distinct = seeds.distinct
    F.fromEither(
      Checks.check(distinct.nonEmpty && size > 0 && size >= distinct.size, "Population size must be positive and hold all distinct seeds")
    ).flatMap { _ =>
      val cloneShare  = if (shuffle) 0.15 else 0.30
      val jitterShare = if (shuffle) 0.55 else 0.60
      val radii       = if (shuffle) Vector(1.0, 3.0, 6.0) else Vector(1.0)
      val clones      = math.max(distinct.size, (size * cloneShare).toInt)
      val jittered    = math.min(size - clones, (size * jitterShare).toInt)
      val copied      = Vector.tabulate(clones)(index => distinct(index % distinct.size))
      for
        neighbours <- Vector.range(0, jittered).traverse(index => jitter(distinct(index % distinct.size), radii(index % radii.size)))
        immigrants <- Vector.range(0, size - clones - jittered).traverse(_ => randomise())
      yield copied ++ neighbours ++ immigrants
    }
  }
}
