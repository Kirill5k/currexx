package currexx.algorithms.progress

import cats.Monad
import cats.syntax.all.*
import currexx.algorithms.{Parameters, ValidatedPopulation}

final class CompositeTracker[F[_], I](
    trackers: List[Tracker[F, I]]
)(using
    F: Monad[F]
) extends Tracker[F, I]:

  override def displayInitial(target: I, params: Parameters[?]): F[Unit] =
    trackers.traverse(_.displayInitial(target, params)).void

  override def displayProgress(progress: Progress[I]): F[Unit] =
    trackers.traverse(_.displayProgress(progress)).void

  override def displayFinal(population: ValidatedPopulation[I]): F[Unit] =
    trackers.traverse(_.displayFinal(population)).void

  override def displayNote(title: String, lines: List[String]): F[Unit] =
    trackers.traverse(_.displayNote(title, lines)).void

object CompositeTracker:
  def make[F[_]: Monad, I](trackers: Tracker[F, I]*): Tracker[F, I] =
    new CompositeTracker[F, I](trackers.toList)
