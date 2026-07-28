package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.Evaluator
import currexx.backtest.services.TestServicesPool
import currexx.backtest.{MarketDataProvider, OrderStats, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.Indicator
import fs2.Stream

object IndicatorEvaluator {

  /** The evaluator a run searches with, together with the backtest underneath it.
    *
    * Fitness collapses a run to a single number and throws away the statistics it came from, so a finished search can say nothing about its
    * own champion beyond its score. Handing back the backtest lets the caller replay one indicator and examine the result properly.
    */
  final case class Evaluation[F[_]](
      evaluator: Evaluator[F, Indicator],
      backtest: Indicator => F[List[OrderStats]]
  )

  def make[F[_]: {Async, Parallel}](
      testFilePaths: List[String],
      strategy: TradeStrategy,
      poolSize: Int,
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      scoringFunction: ScoringFunction = ScoringFunction.Robust()
  ): F[Evaluation[F]] =
    for
      testDataSets <- testFilePaths.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      initialSettings = TestSettings.make(testDataSets.head.head.currencyPair, strategy, otherIndicators)
      pool <- TestServicesPool.make[F](initialSettings, poolSize)
      backtest = (ind: Indicator) =>
        testDataSets.parTraverse { testData =>
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
      eval <- Evaluator.cached[F, Indicator](ind => backtest(ind).map(res => ind -> Fitness(scoringFunction.score(res))))
    yield Evaluation(eval, backtest)
}
