package currexx.backtest

import cats.effect.{IO, IOApp}
import currexx.backtest.services.TestServices
import currexx.core.signal.SignalDetector
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object BatchBacktester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val strategies: List[(String, TestStrategy)] = List(
    "s1_v2_optimized_v2" -> TestStrategy.s1_v2_optimized_v2,
    "s12_optimized"      -> TestStrategy.s12_optimized,
    "s4_optimized"       -> TestStrategy.s4_optimized,
    "s5_optimized_v2"    -> TestStrategy.s5_optimized_v2,

    "s4_regime_optimized_v2" -> TestStrategy.s4_regime_optimized_v2,
    "s1_v2"                  -> TestStrategy.s1_v2,

    "s1"                  -> TestStrategy.s1,
    "s1_optimized"        -> TestStrategy.s1_optimized,
    "s1_v2_optimized"     -> TestStrategy.s1_v2_optimized,
    "s2"                  -> TestStrategy.s2,
    "s2_optimized"        -> TestStrategy.s2_optimized,
    "s2_optimized_v2"     -> TestStrategy.s2_optimized_v2,
    "s4"                  -> TestStrategy.s4,
    "s4_regime"           -> TestStrategy.s4_regime,
    "s4_regime_optimized" -> TestStrategy.s4_regime_optimized,
    "s5"                  -> TestStrategy.s5,
    "s5_optimized"        -> TestStrategy.s5_optimized,
    "s12"                 -> TestStrategy.s12
  )

  val riskSettings: RiskSettings = RiskSettings()

  def runOne(name: String, ts: TestStrategy): IO[String] =
    Stream
      .emits(MarketDataProvider.majors1h)
      .parEvalMap(6) { filePath =>
        val cp       = MarketDataProvider.cpFromFilePath(filePath)
        val settings = TestSettings.make(cp, ts.rules, List(ts.indicator))
        for
          services <- TestServices.make[IO](settings)
          _        <- MarketDataProvider
            .read[IO](filePath)
            .through(services.processMarketData(SignalDetector.pure))
            .compile
            .drain
          orderStats <- services.getOrderStats(riskSettings)
        yield orderStats
      }
      .compile
      .toList
      .map { stats =>
        val portfolio = OrderStats.combine(stats, riskSettings)
        val winPct    = portfolio.winRate * 100
        val drawdown  = portfolio.maxDrawdownPercent
        f"$name%-25s net=${portfolio.totalProfit}%10.5f  closed=${portfolio.total}%5d  open=${portfolio.openPositions.size}%2d  " +
          f"win=${winPct}%6.2f%%  exp=${portfolio.expectancy}%9.6f  PF=${portfolio.profitFactor}%7.3f  " +
          f"DD=${drawdown}%6.2f%%  Sharpe=${portfolio.sharpeRatio}%7.3f  costs=${portfolio.totalCosts}%9.5f"
      }

  override val run: IO[Unit] =
    strategies
      .foldLeft(IO.pure(List.empty[String])) { (acc, kv) =>
        acc.flatMap(lines => runOne(kv._1, kv._2).map(l => lines :+ l))
      }
      .flatMap(lines => IO.println("\n===== BATCH RESULTS =====\n" + lines.mkString("\n")))
}
