package currexx.backtest.walkforward

import currexx.backtest.DataWindow
import currexx.backtest.optimizer.*
import currexx.domain.market.CurrencyPair
import currexx.domain.signal.Indicator
import eu.timepit.refined.types.numeric.PosBigDecimal

import java.time.{Instant, YearMonth}

private[walkforward] object UpgradeFixtures:
  def evidence(indicator: Indicator, net: BigDecimal = 400): SelectionEvidence = SelectionEvidence
    .from(
      candidate = indicator,
      coverage = Map(
        CurrencyPair.fromUnsafe("EUR/USD") -> SelectionCoverage(
          DataWindow(Instant.parse("2024-01-01T00:00:00Z"), Instant.parse("2024-04-30T23:59:59Z")),
          PosBigDecimal.unsafeFrom(BigDecimal(10000))
        )
      ),
      initialBalance = 10000,
      netProfit = net,
      costs = 20,
      maxDrawdownPercent = 1,
      monthlyProfits = (1 to 4).map(month => YearMonth.of(2024, month) -> (net / 4)).toMap,
      score = 1,
      violations = Nil,
      closedTrades = 10,
      forcedClosures = 1
    )
    .toOption
    .get

  def approved(base: Indicator, candidate: Indicator): UpgradeDecision.Approved =
    val original   = evidence(base)
    val challenger = evidence(candidate, 500)
    UpgradeDecision.Approved(
      candidate,
      UpgradeComparison(
        base = original,
        candidate = challenger,
        minimumNetImprovement = 20,
        netImprovement = 100,
        drawdownIncreasePercentagePoints = 0,
        monthlyNetDifferences = original.monthlyProfits.value.keys.map(_ -> BigDecimal(25)).toMap,
        winningMonths = 4,
        stressedBaseNet = 390,
        stressedCandidateNet = 490
      )
    )
