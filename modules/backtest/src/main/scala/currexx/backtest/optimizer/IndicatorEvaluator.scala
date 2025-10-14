package currexx.backtest.optimizer

import cats.{Parallel, Show}
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.Evaluator
import currexx.backtest.services.TestServicesPool
import currexx.backtest.syntax.*
import currexx.backtest.{MarketDataProvider, OrderStatsCollector, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.Indicator
import fs2.Stream

object IndicatorEvaluator {

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
      poolSize: Int = Runtime.getRuntime.availableProcessors()
  ): F[Evaluator[F, Indicator]] =
    for
      testDataSets <- testFilePaths.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      // Create a pool of TestServices for each test dataset to enable parallel evaluation
      pools        <- testDataSets.traverse { testData =>
        val initialSettings = TestSettings.make(testData.head.currencyPair, ts, otherIndicators)
        TestServicesPool.make[F](initialSettings, poolSize).map(pool => testData -> pool)
      }
      eval         <- Evaluator.cached[F, Indicator] { ind =>
        pools
          .parTraverse { case (testData, pool) =>
            pool.use(TestSettings.make(testData.head.currencyPair, ts, ind :: otherIndicators)) { services =>
              for
                _ <- Stream
                  .emits(testData)
                  .through(services.processMarketData(signalDetector))
                  .compile
                  .drain
                orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
              yield orderStats.medianProfitByMonth
            }
          }
          .map(res => ind -> Fitness(res.mean.roundTo(5)))
      }
    yield eval
}
