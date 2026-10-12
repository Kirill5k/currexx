package currexx.backtest

import currexx.backtest.optimizer.indicator.{IndicatorOptimisation, OptimisationRounds}
import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*

import scala.util.Random

object Optimiser extends IOApp.Simple {

  given Random = Random()

  val evaluatorPoolSize = Runtime.getRuntime.availableProcessors()
  val gaParameters      = OptimisationRounds.gaParameters
  val rounds            = OptimisationRounds.rounds

  override def run: IO[Unit] =
    rounds.traverse_ { round =>
      for
        algorithm <- IndicatorOptimisation.make[IO](round, evaluatorPoolSize, StrategyCatalogue.entries)
        _         <- algorithm.optimise
      yield ()
    }
}
