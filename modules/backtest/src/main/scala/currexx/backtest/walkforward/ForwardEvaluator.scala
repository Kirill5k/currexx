package currexx.backtest.walkforward

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import currexx.backtest.{MarketDataProvider, TestSettings, TestStrategy}
import currexx.backtest.MarketDataProvider.Dataset
import currexx.backtest.services.{PeriodSimulation, TestServicesPool}

trait ForwardEvaluator[F[_]]:
  /** Receives the nonempty, common-period datasets already checked by the runner's preflight. */
  def evaluate(candidate: TestStrategy, base: TestStrategy, datasets: List[Dataset]): F[ForwardResult]

object ForwardEvaluator:
  def make[F[_]: {Async, Parallel}](poolSize: Int): ForwardEvaluator[F] = new ForwardEvaluator[F]:
    override def evaluate(candidate: TestStrategy, base: TestStrategy, datasets: List[Dataset]): F[ForwardResult] =
      for
        prepared <- datasets.parTraverse(dataset => MarketDataProvider.read[F](dataset).compile.toList.map(dataset -> _))
        pool     <- TestServicesPool.make[F](TestSettings.make(datasets.head.currencyPair, base.rules, List(base.indicator)), poolSize)
        run = (strategy: TestStrategy) =>
          prepared.parTraverse { case (dataset, data) =>
            pool.use(TestSettings.make(dataset.currencyPair, strategy.rules, List(strategy.indicator))) { services =>
              PeriodSimulation.run(services, data)
            }
          }
        baseStats      <- run(base)
        candidateStats <- if (candidate == base) Async[F].pure(baseStats) else run(candidate)
        _              <- Async[F].raiseWhen((baseStats ++ candidateStats).exists(_.invalidOrderCount != 0))(
          new IllegalStateException("Forward simulation produced invalid orders")
        )
        // Both simulations consume these same bars; coverage does not depend on either strategy's trades.
        coverage = prepared.map { case (dataset, data) =>
          PeriodCoverage(
            dataset.currencyPair.toString,
            data.head.latestTime,
            data(1).latestTime,
            data.last.latestTime.plusNanos(dataset.interval.toDuration.toNanos).minusNanos(1),
            data.head.prices.toList.count(price => dataset.range.exists(_.contains(price.time))) - 1
          )
        }
      yield ForwardResult(ForwardMetrics.fromStats(candidateStats), ForwardMetrics.fromStats(baseStats), coverage)
