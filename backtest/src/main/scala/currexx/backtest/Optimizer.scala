package currexx.backtest

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.traverse.*
import currexx.algorithms.Parameters
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.backtest.optimizer.{IndicatorCrossover, IndicatorEvaluator, IndicatorInitialiser, IndicatorMutator, OptimisationAlgorithm}
import currexx.core.trade.TradeStrategy
import currexx.domain.market.{Indicator, MovingAverage, ValueSource, ValueTransformation}
import fs2.Stream

import scala.util.Random

object Optimizer extends IOApp.Simple {

  given Random = Random()

  val gaParameters = Parameters.GA(
    populationSize = 250,
    maxGen = 250,
    crossoverProbability = 0.7,
    mutationProbability = 0.25,
    elitismRatio = 0.05,
    shuffle = true
  )

  val thresholdCrossing = Indicator.ThresholdCrossing(ValueSource.Close, ValueTransformation.STOCH(15), 80, 20)
  val trendChangeDetection = Indicator.TrendChangeDetection(
    ValueSource.Close,
//    ValueTransformation.SingleOutput.JMA(44, -72, 1)
    ValueTransformation.HMA(25)
//    ValueTransformation.SingleOutput.NMA(45, 5, 11.0d, MovingAverage.Hull)
  )

  val linesCrossing = Indicator.LinesCrossing(
    ValueSource.Close,
    ValueTransformation.JMA(20, 100, 2),
    ValueTransformation.JMA(20, 100, 2)
  )

  val testDataSets    = List("nzd-cad-1d.csv", "eur-chf-1d.csv", "nzd-chf-1d.csv", "eur-aud-1d.csv", "eur-gbp-1d.csv", "aud-cad-1d.csv")
  val strategy        = TradeStrategy.LinesCrossing
  val target          = linesCrossing
  val otherIndicators = Nil

  override def run: IO[Unit] = for
    _       <- IO.println(s"Starting optimization of $target")
    startTs <- IO.monotonic
    init    <- IndicatorInitialiser.make[IO]
    cross   <- IndicatorCrossover.make[IO]
    mut     <- IndicatorMutator.make[IO]
    eval    <- IndicatorEvaluator.make[IO](testDataSets, strategy, otherIndicators)
    sel     <- Selector.rouletteWheel[IO, Indicator]
    elit    <- Elitism.simple[IO, Indicator]
    updateFn = (currentGen: Int, maxGen: Int) => IO.whenA(currentGen % 10 == 0)(IO.println(s"$currentGen out of $maxGen"))
    res   <- OptimisationAlgorithm.ga[IO, Indicator](init, cross, mut, eval, sel, elit, updateFn).optimise(target, gaParameters)
    endTs <- IO.monotonic
    _     <- IO.println(s"Total duration ${(endTs - startTs).toMinutes}m")
    _     <- res.zipWithIndex.take(25).traverse { case ((ind, f), i) => IO.println(s"${i + 1}: $f - $ind") }
  yield ()
}
