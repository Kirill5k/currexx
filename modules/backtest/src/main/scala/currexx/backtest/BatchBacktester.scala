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
    "s1_v2_optimized" -> TestStrategy.s1_v2_optimized,
    "s2_optimized"    -> TestStrategy.s2_optimized,
    "s2_optimized_v2" -> TestStrategy.s2_optimized_v2,
    "s2_optimized_v3" -> TestStrategy.s2_optimized_v3,
    "s2_optimized_v4" -> TestStrategy.s2_optimized_v4,
    "s5_optimized_v2" -> TestStrategy.s5_optimized_v2,
    "s5_optimized_v3" -> TestStrategy.s5_optimized_v3,
    "s6"              -> TestStrategy.s6,
    "s4_optimized_v1" -> TestStrategy.s4_optimized_v1,
    "s4_optimized_v2" -> TestStrategy.s4_optimized_v2,
    "s4_optimized_v3" -> TestStrategy.s4_optimized_v3,
    "s12"             -> TestStrategy.s12,
    "s12_optimized"   -> TestStrategy.s12_optimized,
  )

  val riskSettings: RiskSettings = RiskSettings()

  def runOne(name: String, ts: TestStrategy, datasets: List[MarketDataProvider.Dataset]): IO[String] =
    Stream
      .emits(datasets)
      .parEvalMap(6) { dataset =>
        val settings = TestSettings.make(dataset.currencyPair, ts.rules, List(ts.indicator))
        for
          services <- TestServices.make[IO](settings)
          _        <- MarketDataProvider
            .read[IO](dataset)
            .through(services.processMarketData(SignalDetector.pure))
            .compile
            .drain
          orderStats <- services.getOrderStats(riskSettings)
        yield orderStats
      }
      .compile
      .toList
      .map { stats =>
        val portfolio    = OrderStats.combine(stats)
        val winPct       = portfolio.winRate * 100
        val drawdown     = portfolio.maxDrawdownPercent
        val profitFactor = portfolio.profitFactor.fold("    N/A")(pf => f"$pf%7.3f")
        val sharpe       = portfolio.sharpeRatio.toOption.fold("    N/A")(s => f"$s%7.3f")
        f"$name%-25s net=${portfolio.totalProfit}%10.5f  closed=${portfolio.total}%5d  forced=${portfolio.forcedClosureCount}%2d  " +
          f"win=${winPct}%6.2f%%  exp=${portfolio.expectancy}%9.6f  PF=$profitFactor  " +
          f"DD=${drawdown}%6.2f%%  Sharpe=$sharpe  gross=${portfolio.preCostProfit}%10.5f  " +
          f"costs=${portfolio.totalCosts}%9.5f"
      }

  override val run: IO[Unit] = List(
    "majors 1h 2024-07..2025-07 (12 months, original sample)" -> MarketDataProvider.majors1h,
    "searched 2023-07..2025-07 (24 months, in sample)"        -> MarketDataProvider.majors1hSearched,
    "holdout 2025-12..2026-06 (7 months, never selected)"     -> MarketDataProvider.majors1hHoldout
  ).foldLeft(IO.pure(List.empty[String])) { case (acc, (label, datasets)) =>
    acc.flatMap { sections =>
      strategies
        .foldLeft(IO.pure(List.empty[String])) { (lines, kv) =>
          lines.flatMap(ls => runOne(kv._1, kv._2, datasets).map(l => ls :+ l))
        }
        .map(lines => sections :+ s"--- $label ---\n${lines.mkString("\n")}")
    }
  }.flatMap(sections => IO.println("\n===== BATCH RESULTS =====\n\n" + sections.mkString("\n\n")))
    .flatMap(_ =>
      IO.println("""
          |forced - positions still open when the data ran out, liquidated at the final mark price
          |exp - expectancy - Average net profit per closed trade
          |PF - profit factor - Relationship between winning and losing closed trades (1.5 means $1.50 won for every $1 lost)
          |DD - drawdown - Maximum percentage of the portfolio that was lost during the period
          |sharpe - Risk-adjusted performance calculated from monthly equity returns and annualized. Higher means returns were more consistent
          |""".stripMargin)
    )
}
