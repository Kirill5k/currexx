package currexx.backtest.optimizer

import cats.Show
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.{Evaluator, Initialiser}
import currexx.backtest.{MarketDataProvider, OrderStatsCollector, TestSettings}
import currexx.backtest.services.TestServices
import currexx.core.trade.TradeStrategy
import currexx.domain.market.Currency.{EUR, GBP}
import currexx.domain.market.{CurrencyPair, Indicator, MovingAverage}
import fs2.Stream

object IndicatorEvaluator {

  val cp = CurrencyPair(EUR, GBP)

  given Show[Indicator] = new Show[Indicator] :
    override def show(ind: Indicator): String =
      ind match
        case Indicator.TrendChangeDetection(source, transformation) =>
          s"${ind.kind}-${source.print}-${transformation}"
        case Indicator.ThresholdCrossing(source, transformation, upperBoundary, lowerBoundary) =>
          s"${ind.kind}-${source.print}-${transformation}-lb$lowerBoundary-up$upperBoundary"
  
  def make[F[_]: Async](testFilePath: String, ts: TradeStrategy): F[Evaluator[F, Indicator]] =
    for
      testData <- MarketDataProvider.read[F](testFilePath, cp).compile.toList
      eval <- Evaluator.cached[F, Indicator] { ind =>
        for
          services <- TestServices.make[F](TestSettings.make(cp, ts, ind))
          _ <- Stream
            .emits(testData)
            .through(services.processMarketData)
            .compile
            .drain
          orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
        yield (ind, Fitness(orderStats.totalProfit))
      }
    yield eval
}
