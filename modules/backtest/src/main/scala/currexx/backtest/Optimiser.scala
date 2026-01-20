package currexx.backtest

import cats.Show
import cats.effect.{IO, IOApp}
import currexx.algorithms.{Parameters, ProgressTracker}
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
  val strategy        = TestStrategy.s1

  // Scoring function selection - see SCORING_GUIDE.md for detailed guide
  
  // Recommended: Balanced scoring (combines profit, win/loss ratio, and consistency)
  val scoringFunction = ScoringFunction.balanced(
    profitWeight = 0.4,          // 40% weight on total profit
    ratioWeight = 0.3,           // 30% weight on win/loss ratio
    consistencyWeight = 0.3,     // 30% weight on monthly consistency
    minOrders = Some(50),        // Minimum orders per dataset
    maxOrders = Some(700),       // Maximum orders per dataset
    targetRatio = 2.0            // Target win/loss ratio for normalization
  )
  
  // Alternative options:
  // val scoringFunction = ScoringFunction.riskAdjusted(Some(50), Some(700))
  // val scoringFunction = ScoringFunction.totalProfit
  // val scoringFunction = ScoringFunction.medianWinLossRatio(Some(50), Some(700))
  // val scoringFunction = ScoringFunction.averageMedianProfitByMonth

  override def run: IO[Unit] = for
    _       <- IO.println(s"Starting optimization of ${strategy.indicator}; starting time - ${Instant.now}")
    startTs <- IO.monotonic
    init    <- IndicatorInitialiser.make[IO]
    cross   <- IndicatorCrossover.make[IO]
    mut     <- IndicatorMutator.make[IO]
    eval    <- IndicatorEvaluator.make[IO](testDataSets, strategy.rules, scoringFunction = scoringFunction)
    sel     <- Selector.tournament[IO, Indicator]
    elit    <- Elitism.simple[IO, Indicator]
    prog    <- ProgressTracker.make[IO, Indicator](logInterval = 10, showTopMember = true, showTopN = 3, showStats = false, finalTopN = 25)
    _       <- OptimisationAlgorithm
      .ga[IO, Indicator](init, cross, mut, eval, sel, elit, prog)
      .optimise(strategy.indicator, gaParameters)
    endTs <- IO.monotonic
    _     <- IO.println(s"Total duration ${(endTs - startTs).toMinutes}m")
  yield ()
}
