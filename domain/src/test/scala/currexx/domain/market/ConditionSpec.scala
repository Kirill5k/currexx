package currexx.domain.market

import io.circe.syntax.*
import io.circe.parser.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConditionSpec extends AnyWordSpec with Matchers {

  "A Condition" when {
    "working with codecs" should {
      "decode crossing-up condition from json" in {
        val condition: Condition = Condition.CrossingUp
        val json = """{"kind":"crossing-up"}"""

        condition.asJson.noSpaces mustBe json
        decode[Condition](json) mustBe Right(condition)
      }

      "decode above-threshold condition from json" in {
        val condition: Condition = Condition.AboveThreshold(BigDecimal(10), BigDecimal(20))
        val json = """{"threshold":10,"value":20,"kind":"above-threshold"}"""

        condition.asJson.noSpaces mustBe json
        decode[Condition](json) mustBe Right(condition)
      }

      "decode trend-direction-change condition from json" in {
        val condition: Condition = Condition.TrendDirectionChange(Trend.Upward, Trend.Downward)
        val json = """{"from":"upward","to":"downward","previousTrendLength":null,"kind":"trend-direction-change"}"""

        condition.asJson.noSpaces mustBe json
        decode[Condition](json) mustBe Right(condition)
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
        val line2 = List(1.0, 3.0, 3.0, 3.0, 3.0)

        Condition.linesCrossing(line1, line2) mustBe Some(Condition.CrossingUp)
      }

      "return None when lines do not intersect" in {
        val line1 = List(2.0, 1.0, 1.0, 1.0, 1.0)
        val line2 = List(3.0, 3.0, 3.0, 3.0, 3.0)

        Condition.linesCrossing(line1, line2) mustBe None
      }
    }
  }
}
