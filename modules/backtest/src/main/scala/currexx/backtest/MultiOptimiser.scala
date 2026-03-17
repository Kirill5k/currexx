package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import currexx.algorithms.operators.{Elitism, Selector}
import currexx.algorithms.progress.Tracker
import currexx.backtest.optimizer.{IndicatorEvaluator, IndicatorCrossover, IndicatorInitialiser, IndicatorMutator, OptimisationAlgorithm}
import currexx.backtest.optimizer.IndicatorEvaluator.ScoringFunction
import currexx.domain.signal.Indicator

import scala.util.Random

object MultiOptimiser extends IOApp.Simple {

  given Random = Random()

  val evaluatorPoolSize = Runtime.getRuntime.availableProcessors() * 3

  val rounds: List[OptimisationRound] = List(
    OptimisationRound(
      name = "s1-balanced",
      strategy = TestStrategy.s1,
      gaParameters = Optimiser.gaParameters,
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
    OptimisationRound(
      name = "s1-risk-adjusted",
      strategy = TestStrategy.s1,
      gaParameters = Optimiser.gaParameters,
      scoringFunction = ScoringFunction.riskAdjusted(Some(50), Some(700)),
      testDataSets = MarketDataProvider.majors1h
    )
  )

  override def run: IO[Unit] =
    rounds.traverse_(runRound)

  private def runRound(round: OptimisationRound): IO[Unit] =
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
      prog <- Tracker.markdown[IO, Indicator](
                label = round.name,
                logInterval = 10,
                showTopMember = true,
                showTopN = 3,
                showStats = false,
                finalTopN = 25
              )
      _ <- OptimisationAlgorithm
             .ga[IO, Indicator](init, cross, mut, eval, sel, elit, prog)
             .optimise(round.strategy.indicator, round.gaParameters)
    yield ()
}
