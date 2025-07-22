package currexx.backtest

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.traverse.*
import currexx.algorithms.Parameters
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.domain.signal.Indicator
import currexx.backtest.optimizer.*

import scala.util.Random

object Optimizer extends IOApp.Simple {

  given Random = Random()

  val gaParameters = Parameters.GA(
    populationSize = 250,
    maxGen = 250,
    crossoverProbability = 0.7,
    mutationProbability = 0.2,
    elitismRatio = 0.025,
    shuffle = true
  )

  val testDataSets    = MarketDataProvider.majors
  val strategy        = TestStrategy.s3_rules
  val target          = TestStrategy.s3_indicators.head
  val otherIndicators = TestStrategy.s3_indicators.tail

  override def run: IO[Unit] = for
    _       <- IO.println(s"Starting optimization of $target")
    startTs <- IO.monotonic
    init    <- IndicatorInitialiser.make[IO]
    cross   <- IndicatorCrossover.make[IO]
    mut     <- IndicatorMutator.make[IO]
    eval    <- IndicatorEvaluator.make[IO](testDataSets, strategy, otherIndicators)
    sel     <- Selector.tournament[IO, Indicator]
    elit    <- Elitism.simple[IO, Indicator]
    updateFn = (currentGen: Int, maxGen: Int) => IO.whenA(currentGen % 10 == 0)(IO.println(s"$currentGen out of $maxGen"))
    res   <- OptimisationAlgorithm.ga[IO, Indicator](init, cross, mut, eval, sel, elit, updateFn).optimise(target, gaParameters)
    endTs <- IO.monotonic
    _     <- IO.println(s"Total duration ${(endTs - startTs).toMinutes}m")
    _     <- res.zipWithIndex.take(25).traverse { case ((ind, f), i) => IO.println(s"${i + 1}: $f - $ind") }
  yield ()
}
