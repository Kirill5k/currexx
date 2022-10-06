package currexx.domain.market

import io.circe.syntax.*
import io.circe.parser.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConditionSpec extends AnyWordSpec with Matchers {

  "A Condition" when {
    "working with json codecs" should {
      "decode crossing-up condition from json" in {
        val condition: Condition = Condition.CrossingUp
        val json                 = """{"kind":"crossing-up"}"""

        condition.asJson.noSpaces mustBe json
        decode[Condition](json) mustBe Right(condition)
      }

      "decode above-threshold condition from json" in {
        val condition: Condition = Condition.AboveThreshold(BigDecimal(10), BigDecimal(20))
        val json                 = """{"threshold":10,"value":20,"kind":"above-threshold"}"""

        condition.asJson.noSpaces mustBe json
        decode[Condition](json) mustBe Right(condition)
      }

      "decode trend-direction-change condition from json" in {
        val condition: Condition = Condition.TrendDirectionChange(Trend.Upward, Trend.Downward)
        val json                 = """{"from":"upward","to":"downward","previousTrendLength":null,"kind":"trend-direction-change"}"""

        condition.asJson.noSpaces mustBe json
        decode[Condition](json) mustBe Right(condition)
      }
    }

    "thresholdCrossing" should {
      "return AboveThreshold when current value is above max" in {
        val line = List(5.0, 1.0, 1.0, 1.0, 1.0)

        Condition.thresholdCrossing(line, 1.0, 4.0) mustBe Some(Condition.AboveThreshold(4.0, 5.0))
      }

      "return BelowThreshold when current value is below min" in {
        val line = List(0.5, 1.0, 1.0, 1.0, 1.0)

        Condition.thresholdCrossing(line, 1.0, 4.0) mustBe Some(Condition.BelowThreshold(1.0, 0.5))
      }

      "return None when value is within limits" in {
        val line = List(3.0, 5.0, 1.0, 1.0, 1.0)

        Condition.thresholdCrossing(line, 1.0, 4.0) mustBe None
      }
    }

    "linesCrossing" should {
      "return CrossingDown when line 1 crosses line 2 from above" in {
        val line1 = List(1.0, 3.0, 3.0, 3.0, 3.0)
        val line2 = List(3.0, 1.0, 1.0, 1.0, 1.0)

        Condition.linesCrossing(line1, line2) mustBe Some(Condition.CrossingDown)
      }

      "return CrossingUp when line 1 crosses line 2 from below" in {
        val line1 = List(3.0, 1.0, 1.0, 1.0, 1.0)
        val line2 = List(2.0, 2.0, 2.0, 2.0, 2.0)

        Condition.linesCrossing(line1, line2) mustBe Some(Condition.CrossingUp)
      }

      "return None when lines do not intersect" in {
        val line1 = List(2.0, 1.0, 1.0, 1.0, 1.0)
        val line2 = List(3.0, 3.0, 3.0, 3.0, 3.0)

        Condition.linesCrossing(line1, line2) mustBe None
      }
    }

    "trendDirectionChange" should {
      "return TrendDirectionChange when trend changes from Upward to Consolidation" in {
        val line = List(1.1522, 1.1464, 1.1346, 1.1239, 1.1134, 1.1109, 1.1177, 1.1339, 1.1443, 1.1417, 1.1382, 1.1393)

        Condition.trendDirectionChange(line) mustBe Some(Condition.TrendDirectionChange(Trend.Upward, Trend.Consolidation, Some(4)))
      }

      "return None when trend doesn't change" in {
        val line = List(1.1464, 1.1346, 1.1239, 1.1134, 1.1109, 1.1177, 1.1339, 1.1443, 1.1417, 1.1382, 1.1393)

        Condition.trendDirectionChange(line) mustBe None
      }
    }
  }
}
