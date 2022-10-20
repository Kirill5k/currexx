package currexx.backtest.optimizer

import cats.Show
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.{Evaluator, Initialiser}
import currexx.backtest.{MarketDataProvider, OrderStatsCollector, TestSettings}
import currexx.backtest.services.TestServices
import currexx.backtest.optimizer.syntax.*
import currexx.core.trade.TradeStrategy
import currexx.domain.market.Currency.{EUR, GBP}
import currexx.domain.market.{CurrencyPair, Indicator, MovingAverage}
import fs2.Stream

object IndicatorEvaluator {

  val cp = CurrencyPair(EUR, GBP)

  given Show[Indicator] = (ind: Indicator) => ind match
    case Indicator.TrendChangeDetection(source, transformation) =>
      s"${ind.kind}-${source.print}-${transformation}"
    case Indicator.ThresholdCrossing(source, transformation, upperBoundary, lowerBoundary) =>
      s"${ind.kind}-${source.print}-${transformation}-lb$lowerBoundary-up$upperBoundary"
    case Indicator.LinesCrossing(source, slowTransformation, fastTransformation) =>
      s"${ind.kind}-${source.print}-${slowTransformation}-${fastTransformation}"

  def make[F[_]: Async](
      testFilePaths: List[String],
      ts: TradeStrategy,
      otherIndicators: List[Indicator] = Nil
  ): F[Evaluator[F, Indicator]] =
    for
      testDataSets <- testFilePaths.traverse(MarketDataProvider.read[F](_, cp).compile.toList)
      eval <- Evaluator.cached[F, Indicator] { ind =>
        testDataSets
          .traverse { testData =>
            for
              services <- TestServices.make[F](TestSettings.make(cp, ts, ind :: otherIndicators))
              _ <- Stream
                .emits(testData)
                .through(services.processMarketData)
                .compile
                .drain
              orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
            yield orderStats.totalProfit
          }
          .map(res => ind -> Fitness(res.median))
      }
    yield eval
}
