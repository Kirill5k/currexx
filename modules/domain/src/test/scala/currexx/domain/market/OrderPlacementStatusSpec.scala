package currexx.domain.market

import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OrderPlacementStatusSpec extends AnyWordSpec with Matchers {

  "OrderPlacementStatus codecs" should {
    "encode and decode Success" in {
      val status: OrderPlacementStatus = OrderPlacementStatus.Success
      val json                         = """{"kind":"success"}"""

      status.asJson.noSpaces mustBe json
      decode[OrderPlacementStatus](json) mustBe Right(status)
    }

    "encode and decode Pending" in {
      val status: OrderPlacementStatus = OrderPlacementStatus.Pending
      val json                         = """{"kind":"pending"}"""

      status.asJson.noSpaces mustBe json
      decode[OrderPlacementStatus](json) mustBe Right(status)
    }

    "encode and decode NoPosition" in {
      val status: OrderPlacementStatus = OrderPlacementStatus.NoPosition
      val json                         = """{"kind":"noPosition"}"""

      status.asJson.noSpaces mustBe json
      decode[OrderPlacementStatus](json) mustBe Right(status)
    }

    "encode and decode Cancelled" in {
      val status: OrderPlacementStatus = OrderPlacementStatus.Cancelled("insufficient funds")
      val json                         = """{"reason":"insufficient funds","kind":"cancelled"}"""

      status.asJson.noSpaces mustBe json
      decode[OrderPlacementStatus](json) mustBe Right(status)
    }

    "fail to decode unknown kind" in {
      decode[OrderPlacementStatus]("""{"kind":"unknown"}""").isLeft mustBe true
    }
  }
}
