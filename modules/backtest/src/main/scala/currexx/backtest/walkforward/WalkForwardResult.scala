package currexx.backtest.walkforward

import currexx.backtest.OrderStats

import java.time.Instant

/** Compact financial measurements; completed trades and equity curves stay inside the evaluator. */
final case class ForwardMetrics(
    netProfit: BigDecimal,
    costs: BigDecimal,
    closedTrades: Int,
    forcedClosures: Int,
    profitFactor: Option[BigDecimal],
    maxDrawdownPercent: BigDecimal,
    initialBalance: BigDecimal
)

object ForwardMetrics:
  def fromStats(stats: List[OrderStats]): ForwardMetrics =
    val portfolio = OrderStats.combine(stats)
    ForwardMetrics(
      portfolio.totalProfit,
      portfolio.totalCosts,
      portfolio.total,
      portfolio.forcedClosureCount,
      portfolio.profitFactor,
      portfolio.maxDrawdownPercent,
      portfolio.initialBalance
    )

final case class PeriodCoverage(
    currencyPair: String,
    firstWindow: Instant,
    firstExecution: Instant,
    lastMark: Instant,
    warmupBarsLost: Int
)

final case class ForwardResult(candidate: ForwardMetrics, base: ForwardMetrics, coverage: List[PeriodCoverage]):
  def netDifference: BigDecimal = candidate.netProfit - base.netProfit

final case class WindowResult(window: WalkForwardWindow, seed: Long, selection: FrozenSelection, forward: ForwardResult)
