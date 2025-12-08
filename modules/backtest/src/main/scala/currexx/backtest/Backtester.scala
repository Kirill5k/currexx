package currexx.backtest

import cats.effect.{IO, IOApp}
import currexx.backtest.services.TestServices
import currexx.backtest.syntax.*
import currexx.core.signal.SignalDetector
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Backtester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val testStrategy = TestStrategy.s1

  override val run: IO[Unit] =
    Stream
      .emits(MarketDataProvider.majors1h)
      .parEvalMap(16) { filePath =>
        for
          _ <- IO.println(s"Processing $filePath")
          cp = MarketDataProvider.cpFromFilePath(filePath)
          settings = TestSettings.make(cp, testStrategy.rules, List(testStrategy.indicator))
          services <- TestServices.make[IO](settings)
          _        <- MarketDataProvider
            .read[IO](filePath)
            .through(services.processMarketData(SignalDetector.pure))
            .compile
            .drain
          orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
          _          <- IO.println(s"$cp: ${orderStats.toString}")
        yield orderStats
      }
      .compile
      .toList
      .flatMap { stats =>
        IO.println(
          s"""median win-to-loss ratio: ${stats.map(_.winLossRatio).median},
             |total profit: ${stats.map(_.totalProfit).sum}, 
             |median: ${stats.map(_.totalProfit).median}, 
             |median loss: ${stats.map(_.meanLoss).median}""".stripMargin.replaceAll("\n", "")
        )
      }
}
