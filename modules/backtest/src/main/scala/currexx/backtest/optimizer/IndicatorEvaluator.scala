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

    def averageWinLossRatio(minOrders: Option[Int] = None, maxOrders: Option[Int] = None): ScoringFunction = stats => {
      if (stats.isEmpty) BigDecimal(0)
      else {
        val penalizedRatios = stats.map { os =>
          val numOrders = os.total
          val isBelowMin = minOrders.exists(numOrders < _)
          val isAboveMax = maxOrders.exists(numOrders > _)

          if (isBelowMin || isAboveMax) BigDecimal(0) else os.winLossRatio
        }
        (penalizedRatios.foldLeft(BigDecimal(0))(_ + _) / BigDecimal(stats.size)).roundTo(5)
      }
    }

    val averageMedianProfitByMonth: ScoringFunction = stats =>
      if (stats.isEmpty) BigDecimal(0)
      else (stats.foldLeft(BigDecimal(0))(_ + _.medianProfitByMonth) / BigDecimal(stats.size)).roundTo(5)
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
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      poolSize: Int = Runtime.getRuntime.availableProcessors(),
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
