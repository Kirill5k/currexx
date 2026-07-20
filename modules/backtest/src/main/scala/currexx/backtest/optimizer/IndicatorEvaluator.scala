package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.Evaluator
import currexx.backtest.services.TestServicesPool
import currexx.backtest.{MarketDataProvider, OrderStats, RiskSettings, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.Indicator
import fs2.Stream

object IndicatorEvaluator {

  type ScoringFunction = List[OrderStats] => Double

  object ScoringFunction {
    final case class RobustConfig(
        minClosedTrades: Int = 150,
        minProfitableDatasetRatio: Double = 2.0 / 3.0,
        minProfitFactor: Double = 1.2,
        maxDrawdownPercent: Double = 15.0,
        maxCostToPreCostProfitRatio: Double = 0.4,
        targetNetReturn: Double = 0.5,
        targetRecoveryFactor: Double = 3.0,
        targetSortinoRatio: Double = 2.0,
        targetExpectancyToLossRatio: Double = 0.2
    ) {
      require(minClosedTrades > 0, "Minimum closed trades must be positive")
      require(minProfitableDatasetRatio > 0 && minProfitableDatasetRatio <= 1, "Profitable dataset ratio must be in (0, 1]")
      require(minProfitFactor > 1, "Minimum profit factor must be greater than 1")
      require(maxDrawdownPercent > 0, "Maximum drawdown must be positive")
      require(maxCostToPreCostProfitRatio > 0, "Maximum cost ratio must be positive")
      require(targetNetReturn > 0, "Target net return must be positive")
      require(targetRecoveryFactor > 0, "Target recovery factor must be positive")
      require(targetSortinoRatio > 0, "Target Sortino ratio must be positive")
      require(targetExpectancyToLossRatio > 0, "Target expectancy ratio must be positive")
    }

    /** Scores a candidate on cost-adjusted portfolio performance while rejecting statistically weak or unsafe candidates.
      *
      * Hard gates require enough closed trades, positive expectancy, acceptable drawdown and costs, no invalid orders, and profitability
      * across at least two thirds of datasets. Passing candidates receive a normalized score in [0, 1]:
      *   - 30% net return
      *   - 25% recovery factor
      *   - 15% Sortino ratio
      *   - 15% expectancy relative to average loss
      *   - 15% profitable-dataset ratio
      *
      * Each component is capped at its target so a single outlier cannot dominate selection.
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
            portfolio.profitFactor.toDouble >= config.minProfitFactor
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
            if (portfolio.maxDrawdown == 0) config.targetRecoveryFactor
            else (portfolio.totalProfit / portfolio.maxDrawdown).toDouble
          val expectancyToLoss =
            if (portfolio.averageLoss == 0) config.targetExpectancyToLossRatio
            else (portfolio.expectancy / portfolio.averageLoss).toDouble

          val netReturnScore  = normalized(netReturn, config.targetNetReturn)
          val recoveryScore   = normalized(recoveryFactor, config.targetRecoveryFactor)
          val sortinoScore    = normalized(portfolio.sortinoRatio, config.targetSortinoRatio)
          val expectancyScore = normalized(expectancyToLoss, config.targetExpectancyToLossRatio)

          (0.30 * netReturnScore) +
            (0.25 * recoveryScore) +
            (0.15 * sortinoScore) +
            (0.15 * expectancyScore) +
            (0.15 * profitableRatio)
        }
      }

    private def normalized(value: Double, target: Double): Double =
      math.max(0.0, math.min(value / target, 1.0))
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
