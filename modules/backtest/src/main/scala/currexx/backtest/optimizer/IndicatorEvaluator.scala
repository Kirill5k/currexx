package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.Evaluator
import currexx.backtest.services.TestServicesPool
import currexx.backtest.types.{GreaterThanOne, PositiveUnitInterval}
import currexx.backtest.types.given
import currexx.backtest.{MarketDataProvider, OrderStats, RiskSettings, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.Indicator
import eu.timepit.refined.types.numeric.{PosDouble, PosInt}
import eu.timepit.refined.auto.autoUnwrap
import fs2.Stream

import scala.language.implicitConversions

object IndicatorEvaluator {

  type ScoringFunction = List[OrderStats] => Double

  object ScoringFunction {
    final case class RobustConfig(
        minClosedTrades: PosInt = 150,
        minProfitableDatasetRatio: PositiveUnitInterval = 2.0 / 3.0,
        minProfitFactor: GreaterThanOne = 1.2,
        maxDrawdownPercent: PosDouble = 15.0,
        maxCostToPreCostProfitRatio: PosDouble = 0.4,
        targetNetReturn: PosDouble = 0.5,
        targetRecoveryFactor: PosDouble = 3.0,
        targetSortinoRatio: PosDouble = 2.0,
        targetExpectancyToLossRatio: PosDouble = 0.2
    )

    /** Scores a candidate on cost-adjusted portfolio performance while rejecting statistically weak or unsafe candidates.
      *
      * Hard gates require enough closed trades, positive expectancy, acceptable drawdown and costs, no invalid orders, and profitability
      * across at least two thirds of datasets. Passing candidates receive an unbounded weighted score:
      *   - 30% net return
      *   - 25% recovery factor
      *   - 15% Sortino ratio
      *   - 15% expectancy relative to average loss
      *   - 15% profitable-dataset ratio
      *
      * Targets scale metrics into comparable units: reaching a target contributes that component's nominal weight, while exceeding it
      * continues increasing fitness without an upper bound.
      */
    def robust(config: RobustConfig = RobustConfig()): ScoringFunction = stats =>
      if (stats.isEmpty) 0.0
      else {
        val initialBalance      = stats.head.initialBalance
        val portfolio           = OrderStats.combine(stats, RiskSettings(initialBalance = initialBalance))
        val profitableRatio     = stats.count(_.totalProfit > 0).toDouble / stats.size
        val costToPreCostProfit =
          if (portfolio.preCostProfit <= 0) Double.PositiveInfinity
          else (portfolio.totalCosts / portfolio.preCostProfit).toDouble
        val meetsProfitFactor =
          (portfolio.lossCount == 0 && portfolio.winCount > 0) ||
            portfolio.profitFactor.exists(_.toDouble >= config.minProfitFactor)
        val passesHardConstraints =
          portfolio.total >= config.minClosedTrades &&
            portfolio.totalProfit > 0 &&
            portfolio.expectancy > 0 &&
            meetsProfitFactor &&
            portfolio.maxDrawdownPercent.toDouble <= config.maxDrawdownPercent &&
            profitableRatio >= config.minProfitableDatasetRatio &&
            costToPreCostProfit <= config.maxCostToPreCostProfitRatio &&
            portfolio.invalidOrderCount == 0

        if (!passesHardConstraints) 0.0
        else {
          val netReturn      = (portfolio.totalProfit / initialBalance).toDouble
          val recoveryFactor =
            portfolio.recoveryFactor.fold(config.targetRecoveryFactor: Double)(_.toDouble)
          val expectancyToLoss =
            if (portfolio.averageLoss == 0) config.targetExpectancyToLossRatio: Double
            else (portfolio.expectancy / portfolio.averageLoss).toDouble

          val netReturnScore  = scaled(netReturn, config.targetNetReturn)
          val recoveryScore   = scaled(recoveryFactor, config.targetRecoveryFactor)
          val sortinoScore    = scaled(portfolio.sortinoRatio, config.targetSortinoRatio)
          val expectancyScore = scaled(expectancyToLoss, config.targetExpectancyToLossRatio)

          (0.30 * netReturnScore) +
            (0.25 * recoveryScore) +
            (0.15 * sortinoScore) +
            (0.15 * expectancyScore) +
            (0.15 * profitableRatio)
        }
      }

    private def scaled(value: Double, target: Double): Double =
      math.max(0.0, value / target)
  }

  def make[F[_]: {Async, Parallel}](
      testFilePaths: List[String],
      strategy: TradeStrategy,
      poolSize: Int,
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      scoringFunction: ScoringFunction = ScoringFunction.robust()
  ): F[Evaluator[F, Indicator]] =
    for
      testDataSets <- testFilePaths.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      initialSettings = TestSettings.make(testDataSets.head.head.currencyPair, strategy, otherIndicators)
      pool <- TestServicesPool.make[F](initialSettings, poolSize)
      eval <- Evaluator.cached[F, Indicator] { ind =>
        testDataSets
          .parTraverse { testData =>
            pool.use(TestSettings.make(testData.head.currencyPair, strategy, ind :: otherIndicators)) { services =>
              for
                _ <- Stream
                  .emits(testData)
                  .through(services.processMarketData(signalDetector))
                  .compile
                  .drain
                orderStats <- services.getOrderStats()
              yield orderStats
            }
          }
          .map(res => ind -> Fitness(scoringFunction(res)))
      }
    yield eval
}
