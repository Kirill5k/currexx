package currexx.algorithms.progress

import cats.Monad
import cats.syntax.all.*
import currexx.algorithms.{EvaluatedPopulation, Parameters}

final class CompositeTracker[F[_], I](
    trackers: List[Tracker[F, I]]
)(using
    F: Monad[F]
) extends Tracker[F, I]:

  override def displayInitial(target: I, params: Parameters.GA): F[Unit] =
    trackers.traverse(_.displayInitial(target, params)).void

  override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[I]): F[Unit] =
    trackers.traverse(_.displayProgress(currentGen, maxGen, population)).void

  override def displayFinal(population: EvaluatedPopulation[I]): F[Unit] =
    trackers.traverse(_.displayFinal(population)).void

object CompositeTracker:
  def make[F[_]: Monad, I](trackers: Tracker[F, I]*): Tracker[F, I] =
    new CompositeTracker[F, I](trackers.toList)
