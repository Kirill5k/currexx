package currexx.backtest.walkforward

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import currexx.algorithms.ValidatedPopulation
import currexx.backtest.OptimisationRound
import currexx.backtest.optimizer.OptimisationAlgorithm
import currexx.domain.signal.Indicator

import scala.util.Random

/** Searches only the training and selection corpus supplied by the round. Forward evidence is a separate operation. */
trait WindowSearch[F[_]]:
  def search(round: OptimisationRound, seed: Long): F[ValidatedPopulation[Indicator]]

object WindowSearch:
  def make[F[_]: {Async, Parallel}](poolSize: Int): WindowSearch[F] = new WindowSearch[F]:
    override def search(round: OptimisationRound, seed: Long): F[ValidatedPopulation[Indicator]] =
      Async[F].defer {
        // The initialiser captures this generator during construction. Recreate it and every search cache on each execution.
        given Random = Random(seed)
        OptimisationAlgorithm.indicator[F](round, poolSize).flatMap(_.optimise)
      }
