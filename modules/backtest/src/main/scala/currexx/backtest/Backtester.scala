package currexx.backtest

import cats.effect.{IO, IOApp}
import currexx.backtest.services.TestServices
import currexx.core.signal.SignalDetector
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Backtester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val testStrategy: TestStrategy = TestStrategy.s2
  val riskSettings: RiskSettings = RiskSettings()

  override val run: IO[Unit] =
    Stream
      .emits(MarketDataProvider.majors1h)
      .parEvalMap(16) { filePath =>
        for
          _ <- IO.println(s"Processing $filePath")
          cp       = MarketDataProvider.cpFromFilePath(filePath)
          settings = TestSettings.make(cp, testStrategy.rules, List(testStrategy.indicator))
          services <- TestServices.make[IO](settings)
          _        <- MarketDataProvider
            .read[IO](filePath)
            .through(services.processMarketData(SignalDetector.pure))
            .compile
            .drain
          orderStats <- services.getOrderStats(riskSettings)
          _          <- IO.println(s"$cp: ${orderStats.toString}")
        yield orderStats
      }
      .compile
      .toList
      .flatMap { stats =>
        val portfolio = OrderStats.combine(stats)
        IO.println(s"Portfolio: $portfolio")
      }
}
