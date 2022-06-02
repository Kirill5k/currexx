package currexx.domain.market

import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TradeOrderSpec extends AnyWordSpec with Matchers {

  "TradeOrder codecs" should {
    "decode order from json" in {
      val order: TradeOrder = TradeOrder.Exit

      order.asJson.noSpaces mustBe """{"kind":"exit"}"""
    }

    "encode order to json" in {
      val json = """{"kind":"enter","position":"buy","volume":0.1}"""

      decode[TradeOrder](json) mustBe Right(TradeOrder.Enter(TradeOrder.Position.Buy, BigDecimal(0.1), None, None, None))
    }
  }
}

