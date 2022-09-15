package currexx.domain.market

import io.circe.syntax.*
import io.circe.parser.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConditionSpec extends AnyWordSpec with Matchers {

  "Condition codecs" should {
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
}
