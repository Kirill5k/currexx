package currexx.backtest

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.traverse.*
import currexx.algorithms.Parameters
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.backtest.optimizer.*
import currexx.core.trade.TradeStrategy
import currexx.domain.market.ValueTransformation.*
import currexx.domain.market.{Indicator, ValueSource}

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

  val trendChangeDetection = Indicator.TrendChangeDetection(
    source = ValueSource.Close,
    transformation = sequenced(
      JMA(21, 100, 3),
//      ValueTransformation.RSX(3)
    )
  )

  val linesCrossing = Indicator.LinesCrossing(
    ValueSource.Close,
    HMA(20),
    HMA(10)
  )

  val thresholdCrossing = Indicator.ThresholdCrossing(
    ValueSource.Close,
    RSX(40),
    70D,
    20D
  )

  val keltnerChannel = Indicator.KeltnerChannel(
    ValueSource.Close,
    JMA(45,100,3),
    JMA(9,100,2),
    30,
    1.5
  )

  val testDataSets    = MarketDataProvider.majors
  val strategy        = TradeStrategy.KeltnerChannel
  val target          = keltnerChannel
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
