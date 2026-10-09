package currexx.backtest.optimizer

import currexx.backtest.{CompletedTrade, DataWindow, OrderStats, RiskSettings}
import currexx.domain.market.{Currency, CurrencyPair}
import currexx.domain.market.TradeOrder.Position
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.{Instant, YearMonth}

class SelectionEvidenceSpec extends AnyWordSpec with Matchers {
  import UpgradeFixtures.*

  private val window = DataWindow(Instant.parse("2025-01-01T00:00:00Z"), Instant.parse("2025-03-31T23:59:59.999999999Z"))
  private val scorer = new ScoringFunction {
    override def score(stats: List[OrderStats]): Double                               = stats.map(_.profitByMonth.size).sum.toDouble
    override def violations(stats: List[OrderStats]): List[ScoringFunction.Violation] = Nil
  }

  private def stats(currencyPair: CurrencyPair = pair, terminalAfterWindow: Boolean = false): OrderStats = {
    val closeTime = if (terminalAfterWindow) window.to.plusNanos(1) else window.to
    val trade     = CompletedTrade(
      currencyPair = currencyPair,
      position = Position.Buy,
      openedAt = window.from,
      closedAt = closeTime,
      entryPrice = 1,
      exitPrice = 2,
      volume = 1,
      grossProfit = 110,
      costs = 10,
      netProfit = 100,
      forcedClosure = terminalAfterWindow
    )
    OrderStats.fromTrades(List(trade), RiskSettings(), dataWindow = Some(window))
  }

  private val valid = evidence()

  private def checked(
      coverage: Map[CurrencyPair, SelectionCoverage] = valid.coverage.value,
      initialBalance: BigDecimal = valid.initialBalance.value,
      netProfit: BigDecimal = valid.netProfit,
      costs: BigDecimal = valid.costs.value,
      maxDrawdownPercent: BigDecimal = valid.maxDrawdownPercent.value,
      monthlyProfits: Map[YearMonth, BigDecimal] = valid.monthlyProfits.value,
      score: Double = valid.score.value,
      closedTrades: Int = valid.closedTrades.value,
      forcedClosures: Int = valid.forcedClosures.value
  ): Either[IllegalArgumentException, SelectionEvidence] =
    SelectionEvidence.from(
      baseIndicator,
      coverage,
      initialBalance,
      netProfit,
      costs,
      maxDrawdownPercent,
      monthlyProfits,
      score,
      Nil,
      closedTrades,
      forcedClosures
    )

  "SelectionEvidence" should {
    "prevent unchecked construction and copying" in {
      assertCompiles("currexx.backtest.optimizer.UpgradeFixtures.evidence()")
      assertDoesNotCompile("currexx.backtest.optimizer.UpgradeFixtures.evidence().copy(netProfit = BigDecimal(900))")
      assertDoesNotCompile("""
        val valid = currexx.backtest.optimizer.UpgradeFixtures.evidence()
        new currexx.backtest.optimizer.SelectionEvidence(
          valid.candidate, valid.coverage, valid.initialBalance, valid.netProfit,
          valid.costs, valid.maxDrawdownPercent, valid.monthlyProfits, valid.score,
          valid.violations, valid.closedTrades, valid.forcedClosures)
      """)
      assertDoesNotCompile("""
        val valid = currexx.backtest.optimizer.UpgradeFixtures.evidence()
        currexx.backtest.optimizer.SelectionEvidence(
          valid.candidate, valid.coverage, valid.initialBalance, valid.netProfit,
          valid.costs, valid.maxDrawdownPercent, valid.monthlyProfits, valid.score,
          valid.violations, valid.closedTrades, valid.forcedClosures)
      """)
    }

    "reject invalid scalar measurements through the checked constructor" in {
      List(BigDecimal(0), BigDecimal(-1)).foreach(balance => checked(initialBalance = balance).isLeft mustBe true)
      checked(costs = -1).isLeft mustBe true
      checked(maxDrawdownPercent = -1).isLeft mustBe true
      checked(closedTrades = -1).isLeft mustBe true
      checked(forcedClosures = -1).isLeft mustBe true
      checked(forcedClosures = valid.closedTrades.value + 1).isLeft mustBe true
      List(-1.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { score =>
        checked(score = score).isLeft mustBe true
      }
      checked(score = Double.MaxValue).isRight mustBe true
      checked(costs = 0, maxDrawdownPercent = 0, score = 0, closedTrades = 0, forcedClosures = 0).isRight mustBe true
    }

    "reject incomplete coverage and inconsistent accounting before evidence reaches the policy" in {
      checked(coverage = Map.empty).isLeft mustBe true
      checked(monthlyProfits = Map.empty).isLeft mustBe true
      val covered  = valid.coverage.value(pair)
      val reversed = covered.copy(dataWindow = DataWindow(covered.dataWindow.to, covered.dataWindow.from))
      checked(coverage = Map(pair -> reversed)).isLeft mustBe true
      checked(initialBalance = valid.initialBalance.value + 1).isLeft mustBe true
      checked(netProfit = valid.netProfit + 1).isLeft mustBe true
      val omitted = valid.monthlyProfits.value - YearMonth.of(2025, 1)
      checked(monthlyProfits = omitted, netProfit = omitted.values.sum).isLeft mustBe true
      val extra = valid.monthlyProfits.value.updated(YearMonth.of(2025, 4), BigDecimal(0))
      checked(monthlyProfits = extra).isLeft mustBe true
    }

    "retain signed profits and structural value equality" in {
      val losingMonths = valid.monthlyProfits.value.view.mapValues(-_).toMap
      val losing       = checked(monthlyProfits = losingMonths, netProfit = -valid.netProfit).toOption.get
      losing.netProfit mustBe -valid.netProfit
      val same = checked().toOption.get
      same mustBe valid
      same.hashCode mustBe valid.hashCode
      checked(costs = valid.costs.value + 1).toOption.get must not be valid
    }

    "include flat months and preserve the original absolute score" in {
      val raw      = stats()
      val evidence = SelectionEvidence.fromStats(baseIndicator, List(pair -> raw), scorer).toOption.get
      evidence.monthlyProfits.value mustBe Map(
        YearMonth.of(2025, 1) -> BigDecimal(0),
        YearMonth.of(2025, 2) -> BigDecimal(0),
        YearMonth.of(2025, 3) -> BigDecimal(100)
      )
      evidence.score.value mustBe scorer.score(List(raw))
      evidence.closedTrades.value mustBe 1
      evidence.netProfit mustBe BigDecimal(100)
      evidence.costs.value mustBe BigDecimal(10)
    }

    "assign terminal liquidation to the last covered month" in {
      val raw      = stats(terminalAfterWindow = true)
      val evidence = SelectionEvidence.fromStats(baseIndicator, List(pair -> raw), scorer).toOption.get
      evidence.monthsCovered mustBe 3
      evidence.monthlyProfits.value(YearMonth.of(2025, 3)) mustBe BigDecimal(100)
      evidence.monthlyProfits.value.values.sum mustBe evidence.netProfit
      evidence.score.value mustBe scorer.score(List(raw))
    }

    "match pair identifiers regardless of result order and retain non-trading pairs" in {
      val otherPair = CurrencyPair(Currency.GBP, Currency.USD)
      val flat      = OrderStats.fromTrades(Nil, RiskSettings(), dataWindow = Some(window))
      val input     = List(pair -> stats(), otherPair -> flat)
      val first     = SelectionEvidence.fromStats(baseIndicator, input, scorer)
      val second    = SelectionEvidence.fromStats(baseIndicator, input.reverse, scorer)
      first mustBe second
      first.toOption.get.coverage.value.keySet mustBe Set(pair, otherPair)
      first.toOption.get.initialBalance.value mustBe BigDecimal(20000)
    }

    "fail missing data, missing coverage, duplicate pairs, invalid orders and mismatched trades" in {
      SelectionEvidence.fromStats(baseIndicator, Nil, scorer).isLeft mustBe true
      SelectionEvidence.fromStats(baseIndicator, List(pair -> stats().copy(dataWindow = None)), scorer).isLeft mustBe true
      SelectionEvidence.fromStats(baseIndicator, List(pair -> stats(), pair -> stats()), scorer).isLeft mustBe true
      SelectionEvidence.fromStats(baseIndicator, List(pair -> stats().copy(invalidOrderCount = 1)), scorer).isLeft mustBe true
      val otherPair = CurrencyPair(Currency.GBP, Currency.USD)
      SelectionEvidence.fromStats(baseIndicator, List(otherPair -> stats()), scorer).isLeft mustBe true
    }

    "fail equity outside the covered period unless it is the final forced settlement" in {
      val raw     = stats(terminalAfterWindow = true)
      val invalid = raw.copy(completedTrades = raw.completedTrades.map(_.copy(forcedClosure = false)))
      SelectionEvidence.fromStats(baseIndicator, List(pair -> invalid), scorer).isLeft mustBe true
      SelectionEvidence.fromStats(baseIndicator, List(pair -> stats().copy(equityCurve = Nil)), scorer).isLeft mustBe true
    }

    "accept only negligible decimal accounting residue from converted FX profits" in {
      val net       = BigDecimal(100) / BigDecimal("1.1")
      val traded    = stats().completedTrades.map(_.copy(netProfit = net, grossProfit = net + 10))
      val converted = OrderStats.fromTrades(traded, RiskSettings(), dataWindow = Some(window))
      val result    = SelectionEvidence.fromStats(baseIndicator, List(pair -> converted), scorer)
      result.isRight mustBe true
      result.toOption.get.netProfit mustBe net
      val broken =
        converted.copy(equityCurve = converted.equityCurve.map(point => point.copy(equity = point.equity + BigDecimal("0.000001"))))
      SelectionEvidence.fromStats(baseIndicator, List(pair -> broken), scorer).isLeft mustBe true
    }
  }
}
