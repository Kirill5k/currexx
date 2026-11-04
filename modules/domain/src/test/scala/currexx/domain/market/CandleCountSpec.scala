package currexx.domain.market

import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CandleCountSpec extends AnyWordSpec with Matchers {
  "CandleCount" should {
    "default to 100 and accept both boundaries" in {
      CandleCount.Default.value mustBe 100
      List(1, 80, 250, 4999).foreach { count =>
        CandleCount.from(count).map(_.value) mustBe Right(count)
      }
    }

    "reject counts outside the supported range" in {
      List(Int.MinValue, -1, 0, 5000, Int.MaxValue).foreach { count =>
        CandleCount.from(count).isLeft mustBe true
      }
    }

    "construct valid counts directly with fromUnsafe" in {
      List(1, 100, 250, 4999).foreach { count =>
        CandleCount.fromUnsafe(count).value mustBe count
      }
    }

    "throw the validation error for invalid counts passed to fromUnsafe" in {
      List(Int.MinValue, -1, 0, 5000, Int.MaxValue).foreach { count =>
        val error = intercept[IllegalArgumentException](CandleCount.fromUnsafe(count))
        error.getMessage mustBe "candleCount must be between 1 and 4999"
      }
    }
  }

  "CandleCount codec" should {
    "round trip an integer without a wrapper object" in {
      List(1, 100, 250, 4999).foreach { count =>
        val decoded = decode[CandleCount](count.toString)
        decoded.map(_.value) mustBe Right(count)
        decoded.map(_.asJson.noSpaces) mustBe Right(count.toString)
      }
    }

    "reject invalid and nonnumeric JSON values" in {
      List("0", "-1", "5000", "2147483648", "1.5", "null", "\"100\"", "true", "{}", "[]").foreach { json =>
        withClue(s"$json: ") {
          decode[CandleCount](json).isLeft mustBe true
        }
      }
    }
  }
}
