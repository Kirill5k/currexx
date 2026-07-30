package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.{Evaluator, Validator}
import currexx.backtest.MarketDataProvider.Dataset
import currexx.backtest.services.TestServicesPool
import currexx.backtest.{MarketDataProvider, OrderStats, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.signal.Indicator
import fs2.Stream

object IndicatorObjective {

  /** The two data-bound operators a run searches with, together with the backtests underneath them.
    *
    * They are built together because they are one thing seen twice: the same simulation, the same strategy and the same pool of services,
    * run over two halves of the corpus that must not be confused with one another. Constructing the validator anywhere else would mean
    * handing it a backtest assembled separately, and a validator pointed at the training half is not a validator — it is the search marking
    * its own work, with nothing in any signature to say so.
    *
    * The raw backtests come back alongside the operators because fitness collapses a run to a single number and throws away the statistics
    * it came from. A finished search can say nothing about its own champion beyond its score; replaying one indicator is how the result
    * gets examined properly.
    */
  final case class Operators[F[_]](
      evaluator: Evaluator[F, Indicator],
      validator: Validator[F, Indicator],
      backtest: Indicator => F[List[OrderStats]],
      validate: Indicator => F[List[OrderStats]]
  )

  def make[F[_]: {Async, Parallel}](
      trainingData: List[Dataset],
      strategy: TradeStrategy,
      poolSize: Int,
      shortlistSize: Int,
      validationData: List[Dataset] = Nil,
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      scoringFunction: ScoringFunction = ScoringFunction.Robust()
  ): F[Operators[F]] =
    for
      training   <- trainingData.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      validation <- validationData.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      initialSettings = TestSettings.make(training.head.head.currencyPair, strategy, otherIndicators)
      pool <- TestServicesPool.make[F](initialSettings, poolSize)
      backtest = backtestOver[F](pool, training, strategy, otherIndicators, signalDetector)
      validate = backtestOver[F](pool, validation, strategy, otherIndicators, signalDetector)
      evaluator <- Evaluator.cached[F, Indicator](ind => backtest(ind).map(res => ind -> Fitness(scoringFunction.score(res))))
      validator <- Validator.shortlisted[F, Indicator](shortlistSize)(ind => validate(ind).map(res => Fitness(scoringFunction.score(res))))
    yield Operators(evaluator, validator, backtest, validate)

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
