package currexx.backtest.optimizer

import currexx.backtest.DataWindow
import currexx.backtest.types.{AtLeastOneBigDecimal, PositiveUnitInterval}
import currexx.backtest.types.given
import currexx.domain.market.{Currency, CurrencyPair}
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation}
import eu.timepit.refined.types.numeric.{NonNegBigDecimal, PosBigDecimal, PosInt}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.{YearMonth, ZoneOffset}

private[optimizer] object UpgradeFixtures {
  val pair: CurrencyPair         = CurrencyPair(Currency.EUR, Currency.USD)
  val baseIndicator: Indicator   = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(10))
  val firstIndicator: Indicator  = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(20))
  val secondIndicator: Indicator = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(30))

  def evidence(
      candidate: Indicator = baseIndicator,
      profits: List[BigDecimal] = List(100, 100, 100),
      costs: BigDecimal = 10,
      maxDrawdownPercent: BigDecimal = BigDecimal("0.5"),
      score: Double = 1.0,
      violations: List[ScoringFunction.Violation] = Nil,
      closedTrades: Int = 30,
      forcedClosures: Int = 1,
      currencyPair: CurrencyPair = pair,
      capital: BigDecimal = 10000
  ): SelectionEvidence = {
    val start  = YearMonth.of(2025, 1)
    val window = DataWindow(
      start.atDay(1).atStartOfDay(ZoneOffset.UTC).toInstant,
      start.plusMonths(profits.size).atDay(1).atStartOfDay(ZoneOffset.UTC).toInstant.minusNanos(1)
    )
    SelectionEvidence
      .from(
        candidate = candidate,
        coverage = Map(currencyPair -> SelectionCoverage(window, PosBigDecimal.unsafeFrom(capital))),
        initialBalance = capital,
        netProfit = profits.sum,
        costs = costs,
        maxDrawdownPercent = maxDrawdownPercent,
        monthlyProfits = profits.zipWithIndex.map { case (profit, index) => start.plusMonths(index) -> profit }.toMap,
        score = score,
        violations = violations,
        closedTrades = closedTrades,
        forcedClosures = forcedClosures
      )
      .fold(throw _, identity)
  }
}

class UpgradePolicySpec extends AnyWordSpec with Matchers {
  import UpgradeFixtures.*

  private val base           = evidence()
  private val passingProfits = List[BigDecimal](BigDecimal("107.5"), BigDecimal("107.5"), 100)
  private val candidate      = evidence(firstIndicator, passingProfits)

  private def decide(challengers: SelectionEvidence*): UpgradeDecision =
    UpgradePolicy.decide(base, challengers.toList).fold(error => fail(error), identity)

  private def codes(decision: UpgradeDecision): Set[String] = decision.rejections.flatMap(_.failures.map(_.code)).toSet

  "UpgradePolicy" should {
    "approve equality at net, drawdown, cost stress, and exactly two-thirds winning months" in {
      val result = decide(evidence(firstIndicator, passingProfits, maxDrawdownPercent = BigDecimal("0.60")))
      result mustBe a[UpgradeDecision.Approved]
      val approved = result.asInstanceOf[UpgradeDecision.Approved]
      approved.comparison.netImprovement mustBe BigDecimal(15)
      approved.comparison.minimumNetImprovement mustBe BigDecimal(15)
      approved.comparison.winningMonths mustBe 2
      approved.comparison.stressedNetImprovement mustBe BigDecimal(15)
    }

    "reject a higher fitness candidate that earns less after more trades" in {
      val result = decide(evidence(firstIndicator, List(90, 90, 90), score = 1.0721, closedTrades = 44))
      result mustBe a[UpgradeDecision.RetainBase]
      codes(result) must contain("net-improvement")
    }

    "allow more trades when net improvement survives costs" in {
      decide(evidence(firstIndicator, List(115, 115, 100), closedTrades = 90, costs = 30)) mustBe a[UpgradeDecision.Approved]
    }

    "keep scanning in validation order after a rejected leader and skip the base" in {
      val rejected = evidence(firstIndicator, List(90, 90, 90), score = 4)
      val passing  = evidence(secondIndicator, passingProfits)
      val result   = decide(base, rejected, passing).asInstanceOf[UpgradeDecision.Approved]
      result.candidate mustBe secondIndicator
      result.rejections.map(_.candidate) mustBe List(firstIndicator)
    }

    "select the first approved candidate without applying a second ranking" in {
      val later = evidence(secondIndicator, List(200, 200, 200))
      decide(candidate, later).asInstanceOf[UpgradeDecision.Approved].candidate mustBe firstIndicator
    }

    "retain the base when there is no challenger" in {
      decide(base) mustBe UpgradeDecision.RetainBase(Nil)
      decide() mustBe UpgradeDecision.RetainBase(Nil)
    }

    "treat a tied month as non-winning" in {
      codes(decide(evidence(firstIndicator, List(100, 100, 120)))) must contain("winning-months")
    }

    "apply a configured winning ratio without rounding the required count upward" in {
      val longerBase = evidence(profits = List(100, 100, 100, 100, 100))
      val challenger = evidence(firstIndicator, List(110, 110, 110, 110, 100))
      val result     = UpgradePolicy.decide(longerBase, List(challenger), UpgradePolicyConfig(minWinningMonthRatio = 0.8))
      result.toOption.get mustBe a[UpgradeDecision.Approved]
    }

    "accept exactly five winning months out of six at a five-sixths requirement" in {
      val longerBase = evidence(profits = List.fill(6)(BigDecimal(100)))
      val challenger = evidence(firstIndicator, List(110, 110, 110, 110, 110, 100))
      val config     = UpgradePolicyConfig(minWinningMonthRatio = 5.0 / 6.0)
      val result     = UpgradePolicy.decide(longerBase, List(challenger), config)
      result.toOption.get mustBe a[UpgradeDecision.Approved]

      val fewerWins = evidence(firstIndicator, List(115, 115, 115, 115, 100, 100))
      val rejected  = UpgradePolicy.decide(longerBase, List(fewerWins), config)
      codes(rejected.toOption.get) must contain("winning-months")
    }

    "accept the worst-month boundary and reject any lower value" in {
      val exact = evidence(firstIndicator, List(BigDecimal("112.5"), BigDecimal("112.5"), 90))
      decide(exact) mustBe a[UpgradeDecision.Approved]
      val below = evidence(firstIndicator, List(113, 113, BigDecimal("89.999")))
      codes(decide(below)) must contain("worst-month")
    }

    "reject insufficient evidence as a policy result when coverage matches" in {
      val shortBase      = evidence(profits = List(100, 100))
      val shortCandidate = evidence(firstIndicator, List(110, 110))
      val result         = UpgradePolicy.decide(shortBase, List(shortCandidate)).toOption.get
      codes(result) must contain("months-covered")
    }

    "reject zero score, zero profit and scorer constraint breaches independently" in {
      val violations = List(ScoringFunction.Violation("closed trades", "2", ">= 15"))
      (codes(decide(evidence(firstIndicator, passingProfits, score = 0, violations = violations))) must contain)
        .allOf("absolute-fitness", "absolute-constraint")
      codes(decide(evidence(firstIndicator, List(0, 0, 0)))) must contain("positive-net-profit")
    }

    "reject an improvement that disappears with higher fixed-trade costs" in {
      codes(decide(evidence(firstIndicator, passingProfits, costs = 11))) must contain("stressed-net-improvement")
      codes(decide(evidence(firstIndicator, passingProfits, costs = 630))) must contain("stressed-net-profit")
    }

    "apply the absolute capital margin to zero and negative base profit" in {
      val policy = UpgradePolicyConfig()
      policy.minimumImprovement(0, 10000, 3) mustBe BigDecimal(15)
      policy.minimumImprovement(-100, 10000, 3) mustBe BigDecimal(15)
      val losingBase = evidence(profits = List(-100, -100, -100))
      val lessLosing = evidence(firstIndicator, List(-50, -50, -50))
      val result     = UpgradePolicy.decide(losingBase, List(lessLosing)).toOption.get
      codes(result) must contain("positive-net-profit")
    }

    "use the larger positive-base margin without rounding money" in {
      UpgradePolicyConfig().minimumImprovement(BigDecimal("1000.01"), 10000, 3) mustBe BigDecimal("50.0005")
      codes(decide(evidence(firstIndicator, List(BigDecimal("107.4999"), BigDecimal("107.5"), 100)))) must contain("net-improvement")
    }

    "fail mismatched pairs, dates or capital even after an otherwise passing challenger" in {
      val wrongPair = CurrencyPair(Currency.GBP, Currency.USD)
      val mismatch  = evidence(secondIndicator, passingProfits, currencyPair = wrongPair)
      UpgradePolicy.decide(base, List(candidate, mismatch)).isLeft mustBe true
      val differentDates = evidence(secondIndicator, List(110, 110))
      UpgradePolicy.decide(base, List(differentDates)).isLeft mustBe true
      val differentCapital = evidence(secondIndicator, passingProfits, capital = 20000)
      UpgradePolicy.decide(base, List(differentCapital)).isLeft mustBe true
    }

    "reject invalid settings through refined constructors" in {
      PosBigDecimal.from(BigDecimal(0)).isLeft mustBe true
      PosInt.from(0).isLeft mustBe true
      PositiveUnitInterval.from(0).isLeft mustBe true
      PositiveUnitInterval.from(1.01).isLeft mustBe true
      AtLeastOneBigDecimal.from(BigDecimal("0.9")).isLeft mustBe true
      NonNegBigDecimal.from(BigDecimal(-1)).isLeft mustBe true
      AtLeastOneBigDecimal.from(BigDecimal(1)).map(_.value) mustBe Right(BigDecimal(1))
      PositiveUnitInterval.from(1.0).map(_.value) mustBe Right(1.0)
    }
  }
}
