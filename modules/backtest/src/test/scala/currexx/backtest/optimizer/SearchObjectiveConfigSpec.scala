package currexx.backtest.optimizer

import currexx.backtest.types.OpenUnitInterval
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SearchObjectiveConfigSpec extends AnyWordSpec with Matchers {
  private val baseline  = UpgradeFixtures.evidence()
  private val policy    = UpgradePolicyConfig()
  private val objective = SearchObjectiveConfig.BaselineRelative()

  private def score(net: BigDecimal, quality: Double = 2): Double = objective.adjust(quality, net, baseline, policy)

  "SearchObjectiveConfig" should {
    "keep the current objective and baseline score unchanged" in {
      SearchObjectiveConfig.Current.adjust(2, -500, baseline, policy) mustBe 2.0
      score(baseline.netProfit) mustBe 2.0
    }

    "smoothly reward net improvement with a positive bounded multiplier" in {
      val nets   = List[BigDecimal](-1000000, 299, BigDecimal("299.99"), 300, BigDecimal("300.01"), 301, 1000000)
      val scores = nets.map(score(_))
      scores mustBe scores.sorted
      scores.distinct must have size scores.size
      scores.foreach { adjusted =>
        adjusted must be > 0.0
        adjusted must be >= 1.0
        adjusted must be <= 3.0
      }
      (score(BigDecimal("300.01")) - score(BigDecimal("299.99"))) must be < 0.01
    }

    "preserve a disqualifying zero score even for large profit improvement" in {
      score(1000000, 0) mustBe 0.0
    }

    "use each fold's own capital and months for the margin" in {
      val shorter    = UpgradeFixtures.evidence(profits = List(100))
      val shortScore = objective.adjust(2, 110, shorter, policy)
      val fullScore  = score(310)
      shortScore must be > fullScore
    }

    "reject non-finite weights and both excluded boundaries" in {
      List(0.0, 1.0, -0.1, Double.NaN, Double.PositiveInfinity).foreach { weight =>
        OpenUnitInterval.from(weight).isLeft mustBe true
      }
      OpenUnitInterval.from(0.5).map(_.value) mustBe Right(0.5)
    }
  }
}
