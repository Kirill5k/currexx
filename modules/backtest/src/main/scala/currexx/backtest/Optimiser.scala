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
      name = "s1-wl-ratio",
      strategy = TestStrategy.s1,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.medianWinLossRatio(minOrders = Some(50), maxOrders = Some(1000)),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s1v2-wl-ratio",
      strategy = TestStrategy.s1_v2,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.medianWinLossRatio(minOrders = Some(50), maxOrders = Some(1000)),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s2-wl-ratio",
      strategy = TestStrategy.s2,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.medianWinLossRatio(minOrders = Some(50), maxOrders = Some(1000)),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s2v2-wl-ratio",
      strategy = TestStrategy.s2_v2,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.medianWinLossRatio(minOrders = Some(50), maxOrders = Some(1000)),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s3-wl-ratio",
      strategy = TestStrategy.s3,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.medianWinLossRatio(minOrders = Some(50), maxOrders = Some(1000)),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s4-wl-ratio",
      strategy = TestStrategy.s4,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.medianWinLossRatio(minOrders = Some(50), maxOrders = Some(1000)),
      testDataSets = MarketDataProvider.majors1h
    ),
    OptimisationRound(
      name = "s5-wl-ratio",
      strategy = TestStrategy.s5,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.medianWinLossRatio(minOrders = Some(50), maxOrders = Some(1000)),
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
