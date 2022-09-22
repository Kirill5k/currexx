package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.flatMap.*
import currexx.backtest.services.TestServices
import currexx.core.trade.TradeStrategy
import currexx.domain.market.{CurrencyPair, Indicator, MovingAverage, ValueSource, ValueTransformation}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.Stream
import currexx.domain.market.Currency.{EUR, GBP}

import scala.concurrent.duration.*

object Backtester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val settings = TestSettings.make(
    CurrencyPair(EUR, GBP),
    TradeStrategy.TrendChange,
    List(
      Indicator.TrendChangeDetection(
        source = ValueSource.HL2,
        transformation = ValueTransformation.SingleOutput.sequenced(
//          ValueTransformation.SingleOutput.NMA(2, 5, 3.5, MovingAverage.Weighted),
          ValueTransformation.SingleOutput.HMA(22),
        )
      ),
      Indicator.ThresholdCrossing(
        source = ValueSource.Close,
        transformation = ValueTransformation.DoubleOutput.STOCH(1, 1, 3),
        upperBoundary = 51,
        lowerBoundary = 49
      )
    )
  )

  override val run: IO[Unit] =
    Stream
      .emits(MarketDataProvider.euusDataset)
      .evalMap { filePath =>
        for
          _        <- logger.info(s"Processing $filePath")
          services <- TestServices.make[IO](settings)
          _ <- MarketDataProvider
            .read[IO](filePath, settings.currencyPair)
            .through(services.processMarketData)
            .compile
            .drain
          orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
          _          <- logger.info(orderStats.toString)
        yield orderStats
      }
      .compile
      .toList
      .flatMap(stats => logger.info(s"total profit: ${stats.map(_.totalProfit).sum}"))
}
