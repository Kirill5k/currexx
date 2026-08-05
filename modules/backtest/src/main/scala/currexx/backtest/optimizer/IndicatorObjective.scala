package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.{Evaluator, Validator}
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.services.TestServicesPool
import currexx.backtest.{MarketDataProvider, OrderStats, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.signal.Indicator
import fs2.Stream

object IndicatorObjective {

  /** How a candidate's per-fold scores become the single number selection sorts by.
    *
    * The geometric mean, because a fold aggregate has to answer whether a candidate held up in every regime, not whether it held up on
    * balance: an arithmetic mean lets one fold at four times the target pay outright for a fold worth nothing, which is the
    * single-fitted-regime shape this arrangement exists to refuse. Discounting the mean by the share of folds that earned anything is too
    * coarse to fix it — over three folds that share can only be 0, a third, two thirds or 1.
    *
    * A product has no such blind spot at any number of folds, at the price of a cliff at exactly zero. It holds only while
    * `ScoringFunction` reserves 0.0 for the genuinely disqualifying: no trades, no net profit, non-positive expectancy, a typical period
    * that loses money, a drawdown at twice the limit, or a desynchronised order book. Everything else must ramp, and the concentration
    * constraint is capped at half the score for exactly this reason — a lumpy winner is weak evidence, and one fold's zero here is the
    * whole run's.
    */
  object FoldAggregation {
    def combine(foldScores: List[Double]): Double =
      if (foldScores.isEmpty || foldScores.exists(_ <= 0.0)) 0.0
      else math.pow(foldScores.product, 1.0 / foldScores.size)
  }

  final case class Operators[F[_]](
      evaluator: Evaluator[F, Indicator],
      validator: Validator[F, Indicator],
      backtest: Indicator => F[List[List[OrderStats]]],
      validate: Indicator => F[List[OrderStats]]
  )

  def make[F[_]: {Async, Parallel}](
      corpus: Corpus,
      strategy: TradeStrategy,
      poolSize: Int,
      shortlistSize: Int,
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      scoringFunction: ScoringFunction = ScoringFunction.Robust()
  ): F[Operators[F]] =
    for
      folds      <- corpus.searchFolds.traverse(_.parTraverse(MarketDataProvider.read[F](_).compile.toList))
      validation <- corpus.validationFold.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      initialSettings = TestSettings.make(folds.head.head.head.currencyPair, strategy, otherIndicators)
      pool <- TestServicesPool.make[F](initialSettings, poolSize)
      // Sequential across folds and parallel within one, so that widening the corpus by a fold cannot widen how many
      // backtests contend for the pool: a fold is the same six-pair run the pool was already sized for.
      perFold  = folds.map(fold => backtestOver[F](pool, fold, strategy, otherIndicators, signalDetector))
      backtest = (indicator: Indicator) => perFold.traverse(_(indicator))
      validate = backtestOver[F](pool, validation, strategy, otherIndicators, signalDetector)
      evaluator <- Evaluator.cached[F, Indicator] { indicator =>
        backtest(indicator).map(stats => indicator -> Fitness(FoldAggregation.combine(stats.map(scoringFunction.score))))
      }
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
