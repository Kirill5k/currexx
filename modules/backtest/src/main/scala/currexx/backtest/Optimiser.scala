package currexx.backtest

import cats.Show
import cats.effect.{IO, IOApp}
import currexx.algorithms.Parameters
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.domain.signal.Indicator
import currexx.backtest.optimizer.*
import currexx.backtest.optimizer.IndicatorEvaluator.ScoringFunction

import java.time.Instant
import scala.util.Random

object Optimiser extends IOApp.Simple {

  given Random = Random()

  val gaParameters = Parameters.GA(
    populationSize = 250,
    maxGen = 250,
    crossoverProbability = 0.7,
    mutationProbability = 0.2,
    elitismRatio = 0.025,
    shuffle = true
  )

  val testDataSets    = MarketDataProvider.majors1h
  val strategy        = TestStrategy.s2

  val scoringFunction = ScoringFunction.medianWinLossRatio(Some(50), Some(500))

  override def run: IO[Unit] = for
    _       <- IO.println(s"Starting optimization of ${strategy.indicator}; starting time - ${Instant.now}")
    startTs <- IO.monotonic
    init    <- IndicatorInitialiser.make[IO]
    cross   <- IndicatorCrossover.make[IO]
    mut     <- IndicatorMutator.make[IO]
    eval    <- IndicatorEvaluator.make[IO](testDataSets, strategy.rules, scoringFunction = scoringFunction)
    sel     <- Selector.tournament[IO, Indicator]
    elit    <- Elitism.simple[IO, Indicator]
    prog    <- ProgressTracker.make[IO, Indicator](logInterval = 5, showTopMember = true, showTopN = 3)
    res     <- OptimisationAlgorithm
      .ga[IO, Indicator](init, cross, mut, eval, sel, elit, prog.displayProgress)
      .optimise(strategy.indicator, gaParameters)
    endTs <- IO.monotonic
    _     <- IO.println(s"Total duration ${(endTs - startTs).toMinutes}m")
    _     <- prog.displayFinal(res, topN = 25)
  yield ()
}
