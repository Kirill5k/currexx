package currexx.algorithms.progress

import cats.Monad
import cats.syntax.applicative.*
import currexx.algorithms.{Parameters, ValidatedPopulation}

final class NoopTracker[F[_], I](using
    F: Monad[F]
) extends Tracker[F, I] {

  override def displayInitial(target: I, params: Parameters[?]): F[Unit] = F.unit
  override def displayProgress(progress: Progress[I]): F[Unit]           = F.unit
  override def displayFinal(population: ValidatedPopulation[I]): F[Unit] = F.unit
  override def displayNote(title: String, lines: List[String]): F[Unit]  = F.unit
}

object NoopTracker:
  def make[F[_]: Monad, I]: F[Tracker[F, I]] = new NoopTracker[F, I].pure[F]
