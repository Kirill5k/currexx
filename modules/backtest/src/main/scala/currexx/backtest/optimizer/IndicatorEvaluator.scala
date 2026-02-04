package currexx.backtest.optimizer

import cats.{Parallel, Show}
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.Evaluator
import currexx.backtest.services.TestServicesPool
import currexx.backtest.syntax.*
import currexx.backtest.{MarketDataProvider, OrderStatsCollector, OrderStats, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.Indicator
import fs2.Stream

object IndicatorEvaluator {

  type ScoringFunction = List[OrderStats] => BigDecimal

  object ScoringFunction {
    val totalProfit: ScoringFunction = _.foldLeft(BigDecimal(0))(_ + _.totalProfit).roundTo(5)

    def medianWinLossRatio(minOrders: Option[Int] = None, maxOrders: Option[Int] = None): ScoringFunction = stats => {
      if (stats.isEmpty) BigDecimal(0)
      else {
        // Single-pass filtering and mapping - using List with prepend for O(1) operations
        val validRatios = stats.foldLeft(List.empty[BigDecimal]) { (acc, os) =>
          val numOrders = os.total
          val isBelowMin = minOrders.exists(numOrders < _)
          val isAboveMax = maxOrders.exists(numOrders > _)

          if (isBelowMin || isAboveMax) BigDecimal(0) :: acc else os.winLossRatio :: acc
        }
        validRatios.median
      }
    }

    val averageMedianProfitByMonth: ScoringFunction = stats =>
      if (stats.isEmpty) BigDecimal(0)
      else (stats.foldLeft(BigDecimal(0))(_ + _.medianProfitByMonth) / BigDecimal(stats.size)).roundTo(5)

    /** Balanced scoring function that combines multiple objectives:
      * - Total profit (absolute returns)
      * - Win/loss ratio (trade quality)
      * - Median profit by month (consistency)
      * - Penalizes strategies with too few or too many trades
      *
      * @param profitWeight Weight for total profit component (default: 0.4)
      * @param ratioWeight Weight for win/loss ratio component (default: 0.3)
      * @param consistencyWeight Weight for consistency component (default: 0.3)
      * @param minOrders Minimum number of orders per dataset (penalize if below)
      * @param maxOrders Maximum number of orders per dataset (penalize if above)
      * @param targetRatio Target win/loss ratio for normalization (default: 2.0)
      * @return Weighted composite score
      */
    def balanced(
        profitWeight: Double = 0.4,
        ratioWeight: Double = 0.3,
        consistencyWeight: Double = 0.3,
        minOrders: Option[Int] = Some(30),
        maxOrders: Option[Int] = Some(500),
        targetRatio: Double = 2.0
    ): ScoringFunction = stats => {
      if (stats.isEmpty) BigDecimal(0)
      else {
        // Single-pass calculation of all metrics
        val (validCount, totalProfit, totalWinLossRatio, totalConsistency) = stats.foldLeft((0, BigDecimal(0), BigDecimal(0), BigDecimal(0))) {
          case ((count, profit, ratio, consistency), os) =>
            val numOrders = os.total
            val isBelowMin = minOrders.exists(numOrders < _)
            val isAboveMax = maxOrders.exists(numOrders > _)
            val isValid = !isBelowMin && !isAboveMax

            (
              if (isValid) count + 1 else count,
              profit + os.totalProfit,
              ratio + os.winLossRatio,
              consistency + os.medianProfitByMonth
            )
        }

        val orderCountPenalty = validCount.toDouble / stats.size.toDouble

        // If most strategies violate order constraints, heavily penalize
        if (orderCountPenalty < 0.5) BigDecimal(0)
        else {
          // Component 2: Win/Loss Ratio (normalized and capped)
          val avgWinLossRatio = totalWinLossRatio / BigDecimal(stats.size)
          val normalizedRatio = (avgWinLossRatio / BigDecimal(targetRatio)).min(BigDecimal(1))

          // Component 3: Consistency (median profit by month)
          val avgConsistency = totalConsistency / BigDecimal(stats.size)

          // Combine with weights
          val compositeScore =
            (totalProfit * BigDecimal(profitWeight)) +
            (normalizedRatio * BigDecimal(ratioWeight)) +
            (avgConsistency * BigDecimal(consistencyWeight))

          // Apply order count penalty
          (compositeScore * BigDecimal(orderCountPenalty)).roundTo(5)
        }
      }
    }

    /** Risk-adjusted return scoring that prioritizes profitability while controlling drawdown
      * Uses Sharpe-like ratio: (total profit / risk measure)
      *
      * @param minOrders Minimum number of orders per dataset
      * @param maxOrders Maximum number of orders per dataset
      * @return Score based on risk-adjusted returns
      */
    def riskAdjusted(
        minOrders: Option[Int] = Some(30),
        maxOrders: Option[Int] = Some(500)
    ): ScoringFunction = stats => {
      if (stats.isEmpty) BigDecimal(0)
      else {
        // Single-pass calculation with filtering
        val (validCount, totalProfit, totalBiggestLoss) = stats.foldLeft((0, BigDecimal(0), BigDecimal(0))) {
          case ((count, profit, loss), os) =>
            val numOrders = os.total
            val isBelowMin = minOrders.exists(numOrders < _)
            val isAboveMax = maxOrders.exists(numOrders > _)
            val isValid = !isBelowMin && !isAboveMax

            if (isValid) {
              (count + 1, profit + os.totalProfit, loss + os.biggestLoss.abs)
            } else {
              (count, profit, loss)
            }
        }

        if (validCount == 0) BigDecimal(0)
        else {
          val avgBiggestLoss = totalBiggestLoss / BigDecimal(validCount)

          // Risk-adjusted return: profit / max drawdown
          // Add small epsilon to avoid division by zero
          val epsilon = BigDecimal(0.001)
          (totalProfit / (avgBiggestLoss + epsilon)).roundTo(5)
        }
      }
    }
  }

  given Show[Indicator] = (ind: Indicator) => {
    def showInd(i: Indicator): String = i match
      case Indicator.TrendChangeDetection(vs, transformation) =>
        s"TrendChangeDetection-${vs.print}-${transformation}"
      case Indicator.ThresholdCrossing(vs, transformation, upperBoundary, lowerBoundary) =>
        s"ThresholdCrossing-${vs.print}-${transformation}-lb$lowerBoundary-up$upperBoundary"
      case Indicator.LinesCrossing(vs, slowTransformation, fastTransformation) =>
        s"LinesCrossing-${vs.print}-${slowTransformation}-${fastTransformation}"
      case Indicator.KeltnerChannel(vs, md, atrLength, atrMultiplier) =>
        s"KeltnerChannel-${vs.print}-$md-$atrLength-$atrMultiplier"
      case Indicator.Composite(indicators, combinator) =>
        s"Composite-$combinator-${indicators.map(showInd).toList.mkString("-")}"
      case Indicator.VolatilityRegimeDetection(atrLength, smoothingType, smoothingLength) =>
        s"VolatilityRegimeDetection-${atrLength}-${smoothingType}-$smoothingLength"
      case Indicator.ValueTracking(vs, transformation, targetValue) =>
        s"ValueTracking-${vs.print}-${transformation}-$targetValue"
      case Indicator.PriceLineCrossing(vs, role, transformation) =>
        s"PriceLineCrossing-${vs.print}-$role-$transformation"
      case Indicator.BollingerBands(vs, mb, sdLength, sdMultiplier) =>
        s"BollingerBands-${vs.print}-$mb-$sdLength-$sdMultiplier"
    showInd(ind)
  }

  def make[F[_]: {Async, Parallel}](
      testFilePaths: List[String],
      ts: TradeStrategy,
      poolSize: Int,
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      scoringFunction: ScoringFunction = ScoringFunction.totalProfit
  ): F[Evaluator[F, Indicator]] =
    for
      testDataSets <- testFilePaths.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      initialSettings = TestSettings.make(testDataSets.head.head.currencyPair, ts, otherIndicators)
      pool <- TestServicesPool.make[F](initialSettings, poolSize)
      eval <- Evaluator.cached[F, Indicator] { ind =>
        testDataSets
          .parTraverse { testData =>
            pool.use(TestSettings.make(testData.head.currencyPair, ts, ind :: otherIndicators)) { services =>
              for
                _ <- Stream
                  .emits(testData)
                  .through(services.processMarketData(signalDetector))
                  .compile
                  .drain
                orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
              yield orderStats
            }
          }
          .map(res => ind -> Fitness(scoringFunction(res)))
      }
    yield eval
}
