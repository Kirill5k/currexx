package currexx.backtest.optimizer

import currexx.backtest.OrderStats
import currexx.backtest.optimizer.IndicatorEvaluator.ScoringFunction
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ListMap

class ScoringFunctionSpec extends AnyWordSpec with Matchers {

  def mkStats(
      total: Int = 100,
      profit: BigDecimal = BigDecimal(100),
      losses: List[BigDecimal] = List.fill(25)(BigDecimal(-1)),
      biggestWin: BigDecimal = BigDecimal(10),
      biggestLoss: BigDecimal = BigDecimal(-5),
      profitByMonth: Map[String, BigDecimal] = ListMap(
        "2025-01" -> BigDecimal(10),
        "2025-02" -> BigDecimal(15),
        "2025-03" -> BigDecimal(20)
      )
  ): OrderStats = {
    OrderStats(
      total = total,
      buys = total / 2,
      sells = total / 2,
      losses = losses,
      totalProfit = profit,
      biggestWin = biggestWin,
      biggestLoss = biggestLoss,
      profitByMonth = profitByMonth
    )
  }

  "ScoringFunction.balanced" should {
    "return 0 for empty stats list" in {
      val result = ScoringFunction.balanced()(List.empty)
      result mustBe BigDecimal(0)
    }

    "return 0 when more than 50% of datasets violate order constraints" in {
      val stats = List(
        mkStats(total = 10),  // Below minOrders=50
        mkStats(total = 15),  // Below minOrders=50
        mkStats(total = 100)  // Valid
      )
      val result = ScoringFunction.balanced(minOrders = Some(50))(stats)
      result mustBe BigDecimal(0)
    }

    "calculate score for valid stats with default weights" in {
      val stats = List(
        mkStats(total = 100, profit = BigDecimal(50), losses = List.fill(20)(BigDecimal(-1))),
        mkStats(total = 150, profit = BigDecimal(75), losses = List.fill(30)(BigDecimal(-1)))
      )
      val result = ScoringFunction.balanced()(stats)

      // Should be positive and reasonable
      result must (be > BigDecimal(0) and be < BigDecimal(200)) // Less than total profit due to weighted components
    }

    "prioritize profit when profitWeight is high" in {
      val stats = List(mkStats(total = 100, profit = BigDecimal(100), losses = List.fill(40)(BigDecimal(-1))))

      val highProfitWeight = ScoringFunction.balanced(profitWeight = 0.8, ratioWeight = 0.1, consistencyWeight = 0.1)(stats)
      val lowProfitWeight = ScoringFunction.balanced(profitWeight = 0.2, ratioWeight = 0.4, consistencyWeight = 0.4)(stats)

      highProfitWeight must be > lowProfitWeight
    }

    "prioritize win/loss ratio when ratioWeight is high" in {
      val highRatioStats = List(mkStats(total = 100, profit = BigDecimal(50), losses = List.fill(10)(BigDecimal(-1))))
      val lowRatioStats = List(mkStats(total = 100, profit = BigDecimal(50), losses = List.fill(40)(BigDecimal(-1))))

      val scoring = ScoringFunction.balanced(profitWeight = 0.2, ratioWeight = 0.6, consistencyWeight = 0.2)

      val highRatioScore = scoring(highRatioStats)
      val lowRatioScore = scoring(lowRatioStats)

      highRatioScore must be > lowRatioScore
    }

    "prioritize consistency when consistencyWeight is high" in {
      val consistentStats = List(mkStats(
        total = 100,
        profit = BigDecimal(60),
        profitByMonth = ListMap("2025-01" -> BigDecimal(20), "2025-02" -> BigDecimal(20), "2025-03" -> BigDecimal(20))
      ))

      val inconsistentStats = List(mkStats(
        total = 100,
        profit = BigDecimal(60),
        profitByMonth = ListMap("2025-01" -> BigDecimal(50), "2025-02" -> BigDecimal(5), "2025-03" -> BigDecimal(5))
      ))

      val scoring = ScoringFunction.balanced(profitWeight = 0.2, ratioWeight = 0.2, consistencyWeight = 0.6)

      val consistentScore = scoring(consistentStats)
      val inconsistentScore = scoring(inconsistentStats)

      consistentScore must be > inconsistentScore
    }

    "penalize strategies with too few orders" in {
      val tooFewOrders = List(mkStats(total = 20, profit = BigDecimal(100)))
      val result = ScoringFunction.balanced(minOrders = Some(50))(tooFewOrders)
      result mustBe BigDecimal(0)
    }

    "penalize strategies with too many orders" in {
      val tooManyOrders = List(mkStats(total = 1000, profit = BigDecimal(100)))
      val result = ScoringFunction.balanced(maxOrders = Some(700))(tooManyOrders)
      result mustBe BigDecimal(0)
    }

    "normalize win/loss ratio using targetRatio" in {
      val stats = List(mkStats(total = 100, profit = BigDecimal(50), losses = List.fill(25)(BigDecimal(-1))))

      // With lower targetRatio, the normalized ratio will be higher (capped at 1.0)
      val lowTarget = ScoringFunction.balanced(targetRatio = 1.5, ratioWeight = 1.0, profitWeight = 0, consistencyWeight = 0)(stats)
      val highTarget = ScoringFunction.balanced(targetRatio = 5.0, ratioWeight = 1.0, profitWeight = 0, consistencyWeight = 0)(stats)

      lowTarget must be >= highTarget
    }

    "handle edge case with no losses (perfect win ratio)" in {
      val perfectStats = List(mkStats(total = 100, profit = BigDecimal(100), losses = List.empty))
      val result = ScoringFunction.balanced()(perfectStats)

      result must be > BigDecimal(0)
    }

    "combine multiple datasets correctly" in {
      val stats = List(
        mkStats(total = 100, profit = BigDecimal(50)),
        mkStats(total = 120, profit = BigDecimal(60)),
        mkStats(total = 90, profit = BigDecimal(40))
      )
      val result = ScoringFunction.balanced()(stats)

      // Total profit is 150, should contribute significantly to score
      result must (be > BigDecimal(0) and be < BigDecimal(200)) // But less than raw profit due to other components
    }

    "apply order count penalty proportionally" in {
      val oneInvalid = List(
        mkStats(total = 100),
        mkStats(total = 100),
        mkStats(total = 10)  // Invalid
      )
      val twoInvalid = List(
        mkStats(total = 100),
        mkStats(total = 10),  // Invalid
        mkStats(total = 10)   // Invalid
      )

      val scoring = ScoringFunction.balanced(minOrders = Some(50))

      val oneInvalidScore = scoring(oneInvalid)
      val twoInvalidScore = scoring(twoInvalid)

      // With 2/3 invalid, should get penalized more (actually returns 0)
      twoInvalidScore mustBe BigDecimal(0)
      oneInvalidScore must be > BigDecimal(0) // 1/3 invalid is < 50%
    }
  }

  "ScoringFunction.riskAdjusted" should {
    "return 0 for empty stats list" in {
      val result = ScoringFunction.riskAdjusted()(List.empty)
      result mustBe BigDecimal(0)
    }

    "return 0 when all datasets violate order constraints" in {
      val stats = List(
        mkStats(total = 10),
        mkStats(total = 15)
      )
      val result = ScoringFunction.riskAdjusted(minOrders = Some(50))(stats)
      result mustBe BigDecimal(0)
    }

    "calculate risk-adjusted return correctly" in {
      val stats = List(
        mkStats(total = 100, profit = BigDecimal(100), biggestLoss = BigDecimal(-10))
      )
      val result = ScoringFunction.riskAdjusted()(stats)

      // Score should be approximately profit / loss = 100 / 10 = 10
      result must (be > BigDecimal(9) and be < BigDecimal(11))
    }

    "favor strategies with lower drawdown for same profit" in {
      val lowRisk = List(mkStats(total = 100, profit = BigDecimal(100), biggestLoss = BigDecimal(-5)))
      val highRisk = List(mkStats(total = 100, profit = BigDecimal(100), biggestLoss = BigDecimal(-20)))

      val scoring = ScoringFunction.riskAdjusted()

      val lowRiskScore = scoring(lowRisk)
      val highRiskScore = scoring(highRisk)

      lowRiskScore must be > highRiskScore
    }

    "favor strategies with higher profit for same drawdown" in {
      val highProfit = List(mkStats(total = 100, profit = BigDecimal(200), biggestLoss = BigDecimal(-10)))
      val lowProfit = List(mkStats(total = 100, profit = BigDecimal(100), biggestLoss = BigDecimal(-10)))

      val scoring = ScoringFunction.riskAdjusted()

      val highProfitScore = scoring(highProfit)
      val lowProfitScore = scoring(lowProfit)

      highProfitScore must be > lowProfitScore
    }

    "handle zero loss with epsilon" in {
      val noLossStats = List(mkStats(total = 100, profit = BigDecimal(100), biggestLoss = BigDecimal(0)))
      val result = ScoringFunction.riskAdjusted()(noLossStats)

      // Should not divide by zero, epsilon prevents this
      result must be > BigDecimal(0)
    }

    "filter out datasets violating order constraints" in {
      val stats = List(
        mkStats(total = 100, profit = BigDecimal(100), biggestLoss = BigDecimal(-10)),
        mkStats(total = 10, profit = BigDecimal(50), biggestLoss = BigDecimal(-5))  // Too few orders
      )
      val result = ScoringFunction.riskAdjusted(minOrders = Some(50))(stats)

      // Should only use first dataset: 100 / 10 ≈ 10
      result must (be > BigDecimal(9) and be < BigDecimal(11))
    }

    "combine multiple valid datasets" in {
      val stats = List(
        mkStats(total = 100, profit = BigDecimal(100), biggestLoss = BigDecimal(-10)),
        mkStats(total = 120, profit = BigDecimal(80), biggestLoss = BigDecimal(-8)),
        mkStats(total = 90, profit = BigDecimal(60), biggestLoss = BigDecimal(-6))
      )
      val result = ScoringFunction.riskAdjusted()(stats)

      // Total profit = 240, avg biggest loss = (10+8+6)/3 = 8
      // Score ≈ 240 / 8 = 30
      result must (be > BigDecimal(25) and be < BigDecimal(35))
    }

    "penalize high drawdowns even with good profits" in {
      val lowDrawdown = List(mkStats(total = 100, profit = BigDecimal(100), biggestLoss = BigDecimal(-2)))
      val highDrawdown = List(mkStats(total = 100, profit = BigDecimal(150), biggestLoss = BigDecimal(-50)))

      val scoring = ScoringFunction.riskAdjusted()

      val lowDrawdownScore = scoring(lowDrawdown)  // 100 / 2 = 50
      val highDrawdownScore = scoring(highDrawdown) // 150 / 50 = 3

      lowDrawdownScore must be > highDrawdownScore
    }
  }

  "ScoringFunction.totalProfit" should {
    "return 0 for empty list" in {
      ScoringFunction.totalProfit(List.empty) mustBe BigDecimal(0)
    }

    "sum total profit across all datasets" in {
      val stats = List(
        mkStats(profit = BigDecimal(50)),
        mkStats(profit = BigDecimal(75)),
        mkStats(profit = BigDecimal(25))
      )
      ScoringFunction.totalProfit(stats) mustBe BigDecimal(150)
    }

    "handle negative profits" in {
      val stats = List(
        mkStats(profit = BigDecimal(50)),
        mkStats(profit = BigDecimal(-30))
      )
      ScoringFunction.totalProfit(stats) mustBe BigDecimal(20)
    }
  }

  "ScoringFunction.medianWinLossRatio" should {
    "return 0 for empty list" in {
      ScoringFunction.medianWinLossRatio()(List.empty) mustBe BigDecimal(0)
    }

    "penalize strategies with too few orders" in {
      val stats = List(mkStats(total = 20))
      ScoringFunction.medianWinLossRatio(minOrders = Some(50))(stats) mustBe BigDecimal(0)
    }

    "penalize strategies with too many orders" in {
      val stats = List(mkStats(total = 1000))
      ScoringFunction.medianWinLossRatio(maxOrders = Some(700))(stats) mustBe BigDecimal(0)
    }

    "calculate median win/loss ratio correctly" in {
      val stats = List(
        mkStats(total = 100, losses = List.fill(25)(BigDecimal(-1))),  // Ratio: 75/25 = 3
        mkStats(total = 100, losses = List.fill(50)(BigDecimal(-1)))   // Ratio: 50/50 = 1
      )
      val result = ScoringFunction.medianWinLossRatio()(stats)
      result mustBe BigDecimal(2.0) // Median of [3, 1] = 2
    }
  }

  "ScoringFunction.averageMedianProfitByMonth" should {
    "return 0 for empty list" in {
      ScoringFunction.averageMedianProfitByMonth(List.empty) mustBe BigDecimal(0)
    }

    "calculate average of median monthly profits" in {
      val stats = List(
        mkStats(profitByMonth = ListMap("2025-01" -> BigDecimal(10), "2025-02" -> BigDecimal(20), "2025-03" -> BigDecimal(30))),
        mkStats(profitByMonth = ListMap("2025-01" -> BigDecimal(15), "2025-02" -> BigDecimal(25), "2025-03" -> BigDecimal(35)))
      )
      val result = ScoringFunction.averageMedianProfitByMonth(stats)

      // First dataset median: 20, Second dataset median: 25
      // Average: (20 + 25) / 2 = 22.5
      result mustBe BigDecimal(22.5)
    }
  }
}
