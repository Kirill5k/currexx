package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.optimizer.reporting.RunDiagnostics
import currexx.backtest.services.{PeriodSimulation, TestServicesPool}
import currexx.backtest.{MarketDataProvider, OrderStats, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.signal.Indicator

/** Loads a corpus once and executes its pair simulations through a bounded, reusable services pool. */
final private[optimizer] class IndicatorBacktest[F[_]: {Async, Parallel}] private (
    searchData: List[List[List[MarketTimeSeriesData]]],
    validationData: List[List[MarketTimeSeriesData]],
    pool: TestServicesPool[F],
    strategy: TradeStrategy,
    otherIndicators: List[Indicator],
    signalDetector: SignalDetector,
    diagnostics: Option[RunDiagnostics[F]]
) {
  val hasValidation: Boolean = validationData.nonEmpty

  // Callers sequence folds and reduce their results; parallelism remains limited to pairs within a fold.
  def searchFolds(stage: RunDiagnostics.Stage): List[Indicator => F[List[OrderStats]]] =
    searchData.map(data => indicator => run(data, indicator, stage))

  def validation(indicator: Indicator, stage: RunDiagnostics.Stage): F[List[OrderStats]] =
    run(validationData, indicator, stage)

  private def run(dataSets: List[List[MarketTimeSeriesData]], indicator: Indicator, stage: RunDiagnostics.Stage): F[List[OrderStats]] =
    dataSets.parTraverse { testData =>
      pool.use(TestSettings.make(testData.head.currencyPair, strategy, indicator :: otherIndicators)) { services =>
        for
          _     <- diagnostics.traverse_(_.pairStarted(stage))
          stats <- PeriodSimulation.run(services, testData, signalDetector)
          _     <- diagnostics.traverse_(_.pairCompleted(stage))
        yield stats
      }
    }
}

private[optimizer] object IndicatorBacktest {
  def make[F[_]: {Async, Parallel}](
      corpus: Corpus,
      strategy: TradeStrategy,
      poolSize: Int,
      otherIndicators: List[Indicator],
      signalDetector: SignalDetector,
      diagnostics: Option[RunDiagnostics[F]]
  ): F[IndicatorBacktest[F]] =
    for
      folds      <- corpus.searchFolds.traverse(_.parTraverse(MarketDataProvider.read[F](_).compile.toList))
      validation <- corpus.validationFold.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      settings = TestSettings.make(folds.head.head.head.currencyPair, strategy, otherIndicators)
      pool <- TestServicesPool.make[F](settings, poolSize)
    yield new IndicatorBacktest(folds, validation, pool, strategy, otherIndicators, signalDetector, diagnostics)
}
