package currexx.clients.broker

import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BrokerParametersSpec extends AnyWordSpec with Matchers {

  "BrokerParameters codecs" should {
    "encode and decode Oanda broker parameters to and from json" in {
      val params: BrokerParameters = BrokerParameters.Oanda("my-key", demo = true, "account-123")
      val json                     = """{"apiKey":"my-key","demo":true,"accountId":"account-123","broker":"oanda"}"""

      params.asJson.noSpaces mustBe json
      decode[BrokerParameters](json) mustBe Right(params)
    }

    "fail to decode unknown broker kind" in {
      decode[BrokerParameters]("""{"apiKey":"my-key","demo":true,"accountId":"account-123","broker":"unknown"}""").isLeft mustBe true
    }
  }
}
