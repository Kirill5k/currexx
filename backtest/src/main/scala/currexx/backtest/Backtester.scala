package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.flatMap.*
import currexx.backtest.services.TestServices
import currexx.core.trade.TradeStrategy
import currexx.domain.market.{CurrencyPair, Indicator, MovingAverage, ValueSource, ValueTransformation}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.Stream
import squants.market.{EUR, GBP}

import scala.concurrent.duration.*

object Backtester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val settings = TestSettings.make(
    CurrencyPair(EUR, GBP),
    TradeStrategy.TrendChangeAggressive,
    Indicator.TrendChangeDetection(
      source = ValueSource.Close,
      transformation = ValueTransformation.sequenced(
//        ValueTransformation.NMA(4, 23, 24.0, MovingAverage.Weighted)
        //          ValueTransformation.WMA(5),
//        ValueTransformation.Kalman(0.83),
        ValueTransformation.HMA(5),
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
