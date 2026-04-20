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
    shuffle = true
  )

  val rounds: List[OptimisationRound] = List(
    // --- s1: Trend-following with momentum confirmation ---
    OptimisationRound(
      name = "s1-balanced",
      strategy = TestStrategy.s1,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(
        profitWeight = 0.4,
        ratioWeight = 0.3,
        consistencyWeight = 0.3,
        minOrders = Some(50),
        maxOrders = Some(700),
        targetRatio = 2.0
      ),
      testDataSets = MarketDataProvider.majors1h
    ),
    // --- s2: JMA crossover (mean-reversion), original params ---
    OptimisationRound(
      name = "s2-balanced",
      strategy = TestStrategy.s2,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(minOrders = Some(50), maxOrders = Some(700)),
      testDataSets = MarketDataProvider.majors1h
    ),
    // --- s2_v2: Best-performing strategy (8.07 w/l) — optimize for w/l ratio ---
    OptimisationRound(
      name = "s2v2-wl-ratio",
      strategy = TestStrategy.s2_v2,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.medianWinLossRatio(minOrders = Some(200), maxOrders = Some(600)),
      testDataSets = MarketDataProvider.majors1h
    ),
    // --- s2_v2: Same structure, optimize for risk-adjusted returns ---
    OptimisationRound(
      name = "s2v2-risk-adjusted",
      strategy = TestStrategy.s2_v2,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.riskAdjusted(minOrders = Some(200), maxOrders = Some(600)),
      testDataSets = MarketDataProvider.majors1h
    ),
    // --- s3: Velocity-based breakout — poor baseline, but GA may find viable params ---
    OptimisationRound(
      name = "s3-balanced",
      strategy = TestStrategy.s3,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(minOrders = Some(50), maxOrders = Some(700)),
      testDataSets = MarketDataProvider.majors1h
    ),
    // --- s4: Keltner channel breakout — decent profit (0.176), optimize for more ---
    OptimisationRound(
      name = "s4-profit",
      strategy = TestStrategy.s4,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.totalProfit,
      testDataSets = MarketDataProvider.majors1h
    ),
    // --- s5: Bollinger dual-mode (breakout + reversion) — highest profit (0.263), optimize ratio ---
    OptimisationRound(
      name = "s5-balanced",
      strategy = TestStrategy.s5,
      gaParameters = gaParameters,
      scoringFunction = ScoringFunction.balanced(
        profitWeight = 0.3,
        ratioWeight = 0.4,
        consistencyWeight = 0.3,
        minOrders = Some(50),
        maxOrders = Some(700),
        targetRatio = 2.0
      ),
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
