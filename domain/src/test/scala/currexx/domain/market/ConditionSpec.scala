package currexx.domain.market

import io.circe.syntax.*
import io.circe.parser.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConditionSpec extends AnyWordSpec with Matchers {

  "Condition codecs" should {
    "decode condition from json" in {
      val condition: Condition = Condition.CrossingUp

      condition.asJson.noSpaces mustBe """{"kind":"crossing-up"}"""
    }

    "encode condition to json" in {
      val json = """{"kind":"above-threshold","threshold":10,"value":20}"""

      decode[Condition](json) mustBe Right(Condition.AboveThreshold(BigDecimal(10), BigDecimal(20)))
    }
  }
}
