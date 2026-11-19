package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import currexx.backtest.{MarketDataProvider, OrderStats, TestSettings}
import currexx.backtest.MarketDataProvider.{Corpus, Dataset}
import currexx.backtest.optimizer.reporting.RunDiagnostics
import currexx.backtest.services.{PeriodSimulation, TestServicesPool}
import currexx.core.signal.SignalDetector
import currexx.backtest.TestStrategy
import currexx.domain.market.{CandleCount, CurrencyPair}
import currexx.domain.signal.Indicator

/** Loads a corpus once and executes its pair simulations through a bounded, reusable services pool. */
final private[optimizer] class OptimisationBacktest[F[_]: {Async, Parallel}] private (
    searchData: List[List[PeriodSimulation.Prepared]],
    validationData: List[PeriodSimulation.Prepared],
    pool: TestServicesPool[F],
    otherIndicators: List[Indicator],
    signalDetector: SignalDetector,
    diagnostics: Option[RunDiagnostics[F]]
) {
  val hasValidation: Boolean = validationData.nonEmpty

  // Callers sequence folds and reduce their results; parallelism remains limited to pairs within a fold.
  def searchFolds(stage: RunDiagnostics.Stage): List[TestStrategy => F[List[OrderStats]]] =
    searchFoldsWithPairs(stage).map(run => candidate => run(candidate).map(_.map(_._2)))

  def searchFoldsWithPairs(stage: RunDiagnostics.Stage): List[TestStrategy => F[List[(CurrencyPair, OrderStats)]]] =
    searchData.map(data => candidate => run(data, candidate, stage))

  def validation(candidate: TestStrategy, stage: RunDiagnostics.Stage): F[List[OrderStats]] =
    validationWithPairs(candidate, stage).map(_.map(_._2))

  def validationWithPairs(candidate: TestStrategy, stage: RunDiagnostics.Stage): F[List[(CurrencyPair, OrderStats)]] =
    run(validationData, candidate, stage)

  private def run(
      dataSets: List[PeriodSimulation.Prepared],
      candidate: TestStrategy,
      stage: RunDiagnostics.Stage
  ): F[List[(CurrencyPair, OrderStats)]] =
    dataSets.parTraverse { testData =>
      pool.use(TestSettings.make(testData.currencyPair, candidate.rules, candidate.indicator :: otherIndicators)) { services =>
        for
          _     <- diagnostics.traverse_(_.pairStarted(stage))
          stats <- PeriodSimulation.runPrepared(services, testData, signalDetector)
          _     <- diagnostics.traverse_(_.pairCompleted(stage))
        yield testData.currencyPair -> stats
      }
    }
}

private[optimizer] object OptimisationBacktest {
  def make[F[_]: {Async, Parallel}](
      corpus: Corpus,
      strategy: TestStrategy,
      poolSize: Int,
      candleCount: CandleCount,
      otherIndicators: List[Indicator],
      signalDetector: SignalDetector,
      diagnostics: Option[RunDiagnostics[F]]
  ): F[OptimisationBacktest[F]] =
    for
      _ <- Async[F].raiseWhen(corpus.searchFolds.isEmpty || corpus.searchFolds.exists(_.isEmpty))(
        new IllegalArgumentException("Optimisation requires nonempty search folds")
      )
      folds      <- corpus.searchFolds.traverse(_.parTraverse(readData[F](_, candleCount)))
      validation <- corpus.validationFold.parTraverse(readData[F](_, candleCount))
      settings = TestSettings.make(folds.head.head.currencyPair, strategy.rules, strategy.indicator :: otherIndicators)
      pool <- TestServicesPool.make[F](settings, poolSize)
    yield new OptimisationBacktest(folds, validation, pool, otherIndicators, signalDetector, diagnostics)

  private def readData[F[_]: Async](dataset: Dataset, candleCount: CandleCount): F[PeriodSimulation.Prepared] =
    MarketDataProvider.read[F](dataset, candleCount).compile.toList.flatMap { data =>
      Async[F].fromEither(
        PeriodSimulation
          .prepare(data)
          .left
          .map(_ =>
            new IllegalArgumentException(s"Insufficient bars for ${candleCount.value}-candle warm-up, priming and execution: $dataset")
          )
      )
    }
}
