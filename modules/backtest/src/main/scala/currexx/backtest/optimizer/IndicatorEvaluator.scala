package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.Evaluator
import currexx.backtest.MarketDataProvider.Dataset
import currexx.backtest.services.TestServicesPool
import currexx.backtest.{MarketDataProvider, OrderStats, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.signal.Indicator
import fs2.Stream

object IndicatorEvaluator {

  /** The evaluator a run searches with, together with the two backtests underneath it.
    *
    * Fitness collapses a run to a single number and throws away the statistics it came from, so a finished search can say nothing about its
    * own champion beyond its score. Handing back the backtests lets the caller replay one indicator and examine the result properly.
    *
    * They are separate because they are asking different questions of different data. `backtest` is what the search maximises over, so a
    * score from it says only that a candidate beat the others at fitting the sample every one of them was fitted to. `validate` runs the
    * same simulation over data no candidate was ever scored against, which is the only reading that is evidence of anything, and is
    * therefore what a champion should be chosen by.
    */
  final case class Evaluation[F[_]](
      evaluator: Evaluator[F, Indicator],
      backtest: Indicator => F[List[OrderStats]],
      validate: Indicator => F[List[OrderStats]]
  )

  def make[F[_]: {Async, Parallel}](
      trainingData: List[Dataset],
      strategy: TradeStrategy,
      poolSize: Int,
      validationData: List[Dataset] = Nil,
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      scoringFunction: ScoringFunction = ScoringFunction.Robust()
  ): F[Evaluation[F]] =
    for
      training   <- trainingData.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      validation <- validationData.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      initialSettings = TestSettings.make(training.head.head.currencyPair, strategy, otherIndicators)
      pool <- TestServicesPool.make[F](initialSettings, poolSize)
      backtest = backtestOver[F](pool, training, strategy, otherIndicators, signalDetector)
      validate = backtestOver[F](pool, validation, strategy, otherIndicators, signalDetector)
      eval <- Evaluator.cached[F, Indicator](ind => backtest(ind).map(res => ind -> Fitness(scoringFunction.score(res))))
    yield Evaluation(eval, backtest, validate)

  private def backtestOver[F[_]: {Async, Parallel}](
      pool: TestServicesPool[F],
      dataSets: List[List[MarketTimeSeriesData]],
      strategy: TradeStrategy,
      otherIndicators: List[Indicator],
      signalDetector: SignalDetector
  ): Indicator => F[List[OrderStats]] =
    indicator =>
      dataSets.parTraverse { testData =>
        pool.use(TestSettings.make(testData.head.currencyPair, strategy, indicator :: otherIndicators)) { services =>
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
}
