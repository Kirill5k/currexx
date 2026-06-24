package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import currexx.algorithms.Parameters
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.algorithms.progress.Tracker
import currexx.backtest.optimizer.{IndicatorCrossover, IndicatorEvaluator, IndicatorInitialiser, IndicatorMutator, OptimisationAlgorithm}
import currexx.backtest.optimizer.IndicatorEvaluator.ScoringFunction
import currexx.domain.signal.Indicator

import scala.util.Random

object Optimiser extends IOApp.Simple {

  given Random = Random()

  // Pool size for parallel evaluation.
  // Backtesting replays in-memory market data through pure indicator calculations — CPU-bound,
  // not I/O-bound. Optimal pool size equals available CPU cores to avoid context-switching overhead.
  val evaluatorPoolSize = Runtime.getRuntime.availableProcessors()

  val gaParameters = Parameters.GA(
    populationSize = 250,
    maxGen = 350,
    crossoverProbability = 0.7,
    mutationProbability = 0.2,
    elitismRatio = 0.025,
    shuffle = false
  )

  val rounds: List[OptimisationRound] = List(
    OptimisationRound(
      name = "s6",
      strategy = TestStrategy.s6,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(profitWeight = 0.7, consistencyWeight = 0, minOrders = Some(25), maxOrders = Some(400), targetRatio = 3),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s7",
      strategy = TestStrategy.s7,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(profitWeight = 0.7, consistencyWeight = 0, minOrders = Some(25), maxOrders = Some(400), targetRatio = 3),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s8",
      strategy = TestStrategy.s8,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(profitWeight = 0.7, consistencyWeight = 0, minOrders = Some(25), maxOrders = Some(400), targetRatio = 3),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s9",
      strategy = TestStrategy.s9,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(profitWeight = 0.7, consistencyWeight = 0, minOrders = Some(25), maxOrders = Some(400), targetRatio = 3),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s10",
      strategy = TestStrategy.s10,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(profitWeight = 0.7, consistencyWeight = 0, minOrders = Some(25), maxOrders = Some(400), targetRatio = 3),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s11",
      strategy = TestStrategy.s11,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(profitWeight = 0.7, consistencyWeight = 0, minOrders = Some(25), maxOrders = Some(400), targetRatio = 3),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s12",
      strategy = TestStrategy.s12,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(profitWeight = 0.7, consistencyWeight = 0, minOrders = Some(25), maxOrders = Some(400), targetRatio = 3),
      testDataSets = MarketDataProvider.majors1h
    )
  )

  // Alternative options:
  // val scoringFunction = ScoringFunction.riskAdjusted(Some(50), Some(700))
  // val scoringFunction = ScoringFunction.totalProfit
  // val scoringFunction = ScoringFunction.medianWinLossRatio(Some(50), Some(700))
  // val scoringFunction = ScoringFunction.averageMedianProfitByMonth

  override def run: IO[Unit] =
    rounds.traverse_ { round =>
      for
        init  <- IndicatorInitialiser.make[IO]
        cross <- IndicatorCrossover.make[IO]
        mut   <- IndicatorMutator.make[IO]
        sel   <- Selector.tournament[IO, Indicator]
        elit  <- Elitism.simple[IO, Indicator]
        eval  <- IndicatorEvaluator.make[IO](
          testFilePaths = round.testDataSets,
          strategy = round.strategy.rules,
          poolSize = evaluatorPoolSize,
          scoringFunction = round.scoringFunction
        )
        markDownProg <- Tracker.markdown[IO, Indicator](
          label = round.name,
          logInterval = 10,
          showTopMember = true,
          showTopN = 3,
          showStats = false,
          finalTopN = 25
        )
        loggingProg <- Tracker.logging[IO, Indicator](
          logInterval = 10,
          showTopMember = true,
          showTopN = 3,
          showStats = false,
          finalTopN = 25
        )
        prog = Tracker.composite(markDownProg, loggingProg)
        _ <- OptimisationAlgorithm
          .ga[IO, Indicator](init, cross, mut, eval, sel, elit, prog)
          .optimise(round.strategy.indicator, round.gaParameters)
      yield ()
    }
}
