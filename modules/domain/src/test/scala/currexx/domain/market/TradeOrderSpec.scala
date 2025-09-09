package currexx.domain.market

import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TradeOrderSpec extends AnyWordSpec with Matchers {

  val cp = CurrencyPair(Currency.GBP, Currency.EUR)

  "TradeOrder codecs" should {
    "decode order from json" in {
      val order: TradeOrder = TradeOrder.Exit(cp)

      order.asJson.noSpaces mustBe """{"currencyPair":"GBPEUR","kind":"exit"}"""
    }

    "encode order to json" in {
      val json = """{"kind":"enter","position":"buy","volume":0.1,"currencyPair":"GBPEUR"}"""

      decode[TradeOrder](json) mustBe Right(TradeOrder.Enter(TradeOrder.Position.Buy, cp, BigDecimal(0.1)))
    }
  }
}
