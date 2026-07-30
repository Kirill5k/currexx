package currexx.backtest

import cats.effect.{ExitCode, IO, IOApp}
import currexx.backtest.services.TestServices
import currexx.core.signal.SignalDetector
import currexx.domain.market.{CurrencyPair, TradeOrder as TO}
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.math.BigDecimal.RoundingMode

/** Explains one strategy, where BatchBacktester ranks many.
  *
  * The batch summary line is deliberately one line per strategy, which leaves out everything needed to tell a durable edge from a fitted
  * one. Win rate is the worst offender: on its own it says nothing, because a high rate paired with a payoff ratio below 1 *requires* that
  * rate to hold, whereas a low rate with a high payoff has room to degrade. `breakEvenWinRate` is the single number those two collapse
  * into, and it is the only honest way to compare strategies whose win rates differ.
  *
  * The remaining sections each answer a way a good-looking net figure can mislead: concentration (a handful of trades carried it), forced
  * closures (unrealized marks propped it up), per-month and per-pair spread (it worked in one regime or on one pair), and the long/short
  * split (it was a directional bet on the account currency wearing a strategy's clothes).
  *
  * Usage: `sbt "backtest/runMain currexx.backtest.StrategyAnalyser [name ...]"`, where each name is a key of
  * [[BatchBacktester.strategies]]. With no names it analyses every strategy registered there.
  */
object StrategyAnalyser extends IOApp {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val riskSettings: RiskSettings = RiskSettings()

  private val topTradeCounts = List(5, 10)

  def statsPerPair(ts: TestStrategy): IO[List[(CurrencyPair, OrderStats)]] =
    Stream
      .emits(MarketDataProvider.majors1h)
      .parEvalMap(6) { dataset =>
        val cp       = dataset.currencyPair
        val settings = TestSettings.make(cp, ts.rules, List(ts.indicator))
        for
          services <- TestServices.make[IO](settings)
          _        <- MarketDataProvider
            .read[IO](dataset)
            .through(services.processMarketData(SignalDetector.pure))
            .compile
            .drain
          stats <- services.getOrderStats(riskSettings)
        yield cp -> stats
      }
      .compile
      .toList
      .map(_.sortBy(_._1.toString))

  /** The win rate below which this payoff ratio stops covering its losses, i.e. the distance the hit rate may fall before the edge is gone.
    */
  private def breakEvenWinRate(stats: OrderStats): Option[Double] =
    stats.payoffRatio.map(ratio => 1.0 / (1.0 + ratio.toDouble))

  private def shareOfNet(part: BigDecimal, net: BigDecimal): String =
    if (net == 0) "n/a" else f"${(part / net * 100).toDouble}%.1f%%"

  private def money(value: BigDecimal): String =
    value.setScale(2, RoundingMode.HALF_UP).toString

  private def headline(stats: OrderStats): List[String] =
    List(
      f"net=${money(stats.totalProfit)}%s  trades=${stats.total}%d  forced=${stats.forcedClosureCount}%d  " +
        f"win=${(stats.winRate * 100).toDouble}%.2f%%  expectancy=${money(stats.expectancy)}%s",
      f"avgWin=${money(stats.averageWin)}%s  avgLoss=${money(stats.averageLoss)}%s  " +
        f"payoff=${stats.payoffRatio.fold("n/a")(r => f"${r.toDouble}%.3f")}%s  " +
        f"breakEvenWin=${breakEvenWinRate(stats).fold("n/a")(r => f"${r * 100}%.1f%%")}%s",
      f"PF=${stats.profitFactor.fold("n/a")(v => f"${v.toDouble}%.3f")}%s  " +
        f"DD=${stats.maxDrawdownPercent.toDouble}%.2f%%  " +
        f"biggestWin=${money(stats.biggestWin)}%s  biggestLoss=${money(stats.biggestLoss)}%s  " +
        f"maxConsecLosses=${stats.maxConsecutiveLosses}%d  maxConsecWins=${stats.maxConsecutiveWins}%d"
    )

  /** How much of the net a few trades carried. A strategy whose net survives the removal of its best trades is repeatable; one whose does
    * not was a handful of lucky entries wearing 200 trades as camouflage.
    */
  private def concentration(stats: OrderStats): List[String] = {
    val descending = stats.completedTrades.map(_.netProfit).sortBy(net => -net)
    topTradeCounts.filter(_ < stats.total).map { n =>
      val top = descending.take(n).sum
      f"top$n%-3d = ${money(top)}%12s (${shareOfNet(top, stats.totalProfit)}%6s of net)   net excluding top$n = ${money(
          stats.totalProfit - top
        )}%s"
    }
  }

  /** Positions still open when the data ran out were liquidated at the final mark rather than by a rule, so a net that leans on them is
    * reporting a bet that never actually closed.
    */
  private def forcedClosures(stats: OrderStats): String = {
    val forced = stats.completedTrades.filter(_.forcedClosure)
    val net    = forced.map(_.netProfit).sum
    f"${forced.size}%d trades, net=${money(net)}%s (${shareOfNet(net, stats.totalProfit)}%s of net)"
  }

  private def monthly(stats: OrderStats): List[String] = {
    val months = stats.profitByMonth.toList.sortBy(_._1)
    val worst  = months.minByOption(_._2).fold("n/a") { case (month, profit) => s"$month ${money(profit)}" }
    List(
      f"profitable ${months.count(_._2 > 0)}%d/${months.size}%d months   worst=$worst%s",
      months.map { case (month, profit) => f"$month%s:${profit.toDouble}%,.0f" }.mkString("  ")
    )
  }

  /** A near-even split is evidence the rules read the market; a lopsided one means the result rode a single directional move. */
  private def bySide(stats: OrderStats): List[String] =
    List(TO.Position.Buy, TO.Position.Sell).map { position =>
      val trades   = stats.completedTrades.filter(_.position == position)
      val wins     = trades.count(_.netProfit > 0)
      val net      = trades.map(_.netProfit).sum
      val holdHrs  = trades.map(t => (t.closedAt.toEpochMilli - t.openedAt.toEpochMilli).toDouble / 3600000.0)
      val avgHold  = if (holdHrs.isEmpty) 0.0 else holdHrs.sum / holdHrs.size
      val winPct   = if (trades.isEmpty) 0.0 else wins * 100.0 / trades.size
      val sideName = if (position == TO.Position.Buy) "long" else "short"
      f"$sideName%-5s trades=${trades.size}%4d  net=${money(net)}%12s  win=$winPct%6.2f%%  avgHold=$avgHold%7.1fh"
    }

  private def perPair(statsPerPair: List[(CurrencyPair, OrderStats)]): List[String] =
    statsPerPair.map { case (cp, s) =>
      val pf = s.profitFactor.fold("n/a")(v => f"${v.toDouble}%.3f")
      f"$cp%-8s net=${money(s.totalProfit)}%10s  trades=${s.total}%4d  win=${(s.winRate * 100).toDouble}%6.2f%%  " +
        f"PF=$pf%7s  DD=${s.maxDrawdownPercent.toDouble}%5.2f%%  forced=${s.forcedClosureCount}%d"
    }

  def report(name: String, statsPerPair: List[(CurrencyPair, OrderStats)]): String = {
    val portfolio = OrderStats.combine(statsPerPair.map(_._2))
    val sections  = List(
      ""              -> headline(portfolio),
      "concentration" -> concentration(portfolio),
      "forced"        -> List(forcedClosures(portfolio)),
      "monthly"       -> monthly(portfolio),
      "by side"       -> bySide(portfolio),
      "per pair"      -> perPair(statsPerPair)
    )
    val body = sections.map { case (title, lines) =>
      val heading = if (title.isEmpty) "" else s"  $title:\n"
      heading + lines.map(line => s"    $line").mkString("\n")
    }
    s"=== $name\n${body.mkString("\n")}"
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val catalogue = BatchBacktester.strategies
    val unknown   = args.filterNot(name => catalogue.exists(_._1 == name))
    if (unknown.nonEmpty)
      IO.println(s"Unknown strategy: ${unknown.mkString(", ")}. Known: ${catalogue.map(_._1).mkString(", ")}").as(ExitCode.Error)
    else {
      val selected = if (args.isEmpty) catalogue else catalogue.filter(kv => args.contains(kv._1))
      selected
        .foldLeft(IO.pure(List.empty[String])) { (acc, kv) =>
          acc.flatMap(reports => statsPerPair(kv._2).map(stats => reports :+ report(kv._1, stats)))
        }
        .flatMap(reports => IO.println("\n===== STRATEGY ANALYSIS =====\n" + reports.mkString("\n\n")))
        .as(ExitCode.Success)
    }
  }
}
