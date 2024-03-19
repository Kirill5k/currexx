package currexx.backtest.optimizer

import cats.Show
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.Evaluator
import currexx.backtest.services.TestServices
import currexx.backtest.syntax.*
import currexx.backtest.{MarketDataProvider, OrderStatsCollector, TestSettings}
import currexx.core.trade.TradeStrategy
import currexx.domain.market.Currency.{EUR, GBP}
import currexx.domain.market.{CurrencyPair, Indicator}
import fs2.Stream

object IndicatorEvaluator {

  val cp = CurrencyPair(EUR, GBP)

  given Show[Indicator] = (ind: Indicator) =>
    ind match
      case Indicator.TrendChangeDetection(vs, transformation) =>
        s"${ind.kind}-${vs.print}-${transformation}"
      case Indicator.ThresholdCrossing(vs, transformation, upperBoundary, lowerBoundary) =>
        s"${ind.kind}-${vs.print}-${transformation}-lb$lowerBoundary-up$upperBoundary"
      case Indicator.LinesCrossing(vs, slowTransformation, fastTransformation) =>
        s"${ind.kind}-${vs.print}-${slowTransformation}-${fastTransformation}"
      case Indicator.KeltnerChannel(vs, vs1, vs2, atrLength, atrMultiplier) =>
        s"${ind.kind}-${vs.print}-$vs1-$vs2-$atrLength-$atrMultiplier"

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
            yield orderStats.medianProfitByMonth
          }
          .map(res => ind -> Fitness(res.mean.roundTo(5)))
      }
    yield eval
}
