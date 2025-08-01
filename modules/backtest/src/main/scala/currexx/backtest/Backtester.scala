package currexx.backtest

import cats.effect.{IO, IOApp}
import currexx.backtest.services.TestServices
import currexx.backtest.syntax.*
import currexx.core.signal.ValueTransformer
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Backtester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override val run: IO[Unit] =
    Stream
      .emits(MarketDataProvider.majors1h)
      .parEvalMap(16) { filePath =>
        for
          _ <- logger.info(s"Processing $filePath")
          settings = TestSettings.make(
            MarketDataProvider.cpFromFilePath(filePath),
            TestStrategy.s1_rules,
            TestStrategy.s1_indicators
          )
          services <- TestServices.make[IO](settings)
          _        <- MarketDataProvider
            .read[IO](filePath)
            .through(services.processMarketData(ValueTransformer.pure))
            .compile
            .drain
          orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
          _          <- logger.info(orderStats.toString)
        yield orderStats
      }
      .compile
      .toList
      .flatMap { stats =>
        logger.info(
          s"total profit: ${stats.map(_.totalProfit).sum}, median: ${stats.map(_.totalProfit).median}, median loss: ${stats.map(_.meanLoss).median}"
        )
      }
}
