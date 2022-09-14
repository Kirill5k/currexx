package currexx.backtest

import cats.Show
import cats.effect.{IO, IOApp}
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
    populationSize = 120,
    maxGen = 200,
    crossoverProbability = 0.7,
    mutationProbability = 0.2,
    elitismRatio = 0.2,
    shuffle = true
  )

  val otherIndicators = List(
    Indicator.TrendChangeDetection(
      ValueSource.Close,
      //    ValueTransformation.SingleOutput.HMA(25)
      ValueTransformation.SingleOutput.NMA(45, 5, 11.0d, MovingAverage.Weighted)
    )
  )

  val target = Indicator.ThresholdCrossing(ValueSource.Close, ValueTransformation.DoubleOutput.STOCH(15, 3, 3), 80, 20)

  override def run: IO[Unit] = for
    startTs <- IO.realTime
    init    <- IndicatorInitialiser.make[IO]
    cross   <- IndicatorCrossover.make[IO]
    mut     <- IndicatorMutator.make[IO]
    eval    <- IndicatorEvaluator.make[IO]("eur-chf-1d.csv", TradeStrategy.TrendChangeWithConfirmation, otherIndicators)
    sel     <- Selector.rouletteWheel[IO, Indicator]
    elit    <- Elitism.simple[IO, Indicator]
    updateFn = (currentGen: Int, maxGen: Int) => IO.whenA(currentGen % 10 == 0)(IO.println(s"$currentGen out of $maxGen"))
    res   <- OptimisationAlgorithm.ga[IO, Indicator](init, cross, mut, eval, sel, elit, updateFn).optimise(target, gaParameters)
    endTs <- IO.realTime
    _     <- IO.println(s"Total duration: ${(endTs - startTs).toMinutes}m")
    _     <- IO.println(s"${res._1} - ${res._2}")
  yield ()
}
