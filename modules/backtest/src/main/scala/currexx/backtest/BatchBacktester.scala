package currexx.backtest

import cats.effect.{IO, IOApp}
import currexx.backtest.services.TestServices
import currexx.core.signal.SignalDetector
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object BatchBacktester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  /** The vals worth the runtime, including research candidates, with selection history recorded on each val.
    *
    * Not all of `TestStrategy`, which keeps every val a decision was ever based on so that a report filename still resolves to something. A
    * val is dropped from here once a descendant dominates it on the holdout, or once its family has been answered - measuring it again only
    * adds a line nothing reads. Each of those carries a `Not in BatchBacktester` line in its comment saying which val replaced it.
    */
  val strategies: List[(String, TestStrategy)] = List(
    "s2_optimized_v4" -> TestStrategy.s2_optimized_v4,
    "s2_optimized_v3" -> TestStrategy.s2_optimized_v3,
    "s2_optimized"    -> TestStrategy.s2_optimized,
    "s10_v2"          -> TestStrategy.s10_v2,
    "s5_optimized_v2" -> TestStrategy.s5_optimized_v2,
    "s5_optimized_v3" -> TestStrategy.s5_optimized_v3,
    "s1_v2_optimized" -> TestStrategy.s1_v2_optimized,
    "s4_optimized_v2" -> TestStrategy.s4_optimized_v2,
    "s6_optimized"    -> TestStrategy.s6_optimized,
    "s6"              -> TestStrategy.s6
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

  /** The searched years separately as well as pooled, because the pooled figure hides which of them paid for the other.
    *
    * A val that nets +6000 over the two might have earned +7600 in one and lost -1600 in the other, and a single number cannot show that -
    * see the note in `TestStrategy` on what the 2023-24 year separates. The later evaluation is last; s10_v2 has reused it for development.
    */
  override val run: IO[Unit] = List(
    "searched 2023-07..2024-06 (12 months, in sample)"                      -> MarketDataProvider.majors1h_202307_202406,
    "searched 2024-07..2025-07 (12 months, in sample)"                      -> MarketDataProvider.majors1h,
    "searched 2023-07..2025-07 (24 months, in sample)"                      -> MarketDataProvider.majors1hSearched,
    "historical 2025-12..2026-06 (7 months, reused for s10_v2 development)" -> MarketDataProvider.majors1hHoldout
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
          |DD - drawdown - Largest peak-to-trough decline in portfolio equity at candle closes and executions, including open P&L and costs
          |sharpe - Risk-adjusted performance calculated from monthly equity returns and annualized. Higher means returns were more consistent
          |""".stripMargin)
    )
}
