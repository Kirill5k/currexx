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
    populationSize = 100,
    maxGen = 200,
    crossoverProbability = 0.7,
    mutationProbability = 0.2,
    elitismRatio = 0.4,
    shuffle = true
  )

  val target = Indicator.TrendChangeDetection(
    ValueSource.Close,
    ValueTransformation.SingleOutput.HMA(25)
//    ValueTransformation.SingleOutput.NMA(43, 12, 8.0d, MovingAverage.Weighted)
  )

  override def run: IO[Unit] = for
    init  <- IndicatorInitialiser.make[IO]
    cross <- IndicatorCrossover.make[IO]
    mut   <- IndicatorMutator.make[IO]
    eval  <- IndicatorEvaluator.make[IO]("eur-gbp-1d.csv", TradeStrategy.TrendChangeAggressive)
    sel   <- Selector.rouletteWheel[IO, Indicator]
    elit  <- Elitism.simple[IO, Indicator]
    res   <- OptimisationAlgorithm.ga[IO, Indicator](init, cross, mut, eval, sel, elit).optimise(target, gaParameters)
    _     <- IO.println(s"${res._1} - ${res._2}")
  yield ()
}
