package currexx.domain.monitor

import io.circe.DecodingFailure
import io.circe.parser.decode
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LimitsSpec extends AnyWordSpec with Matchers {

  "A Limits Codec" should {
    "not allow to create limits with empty fields" in {
      val result = decode[Limits]("{}")

      result mustBe Left(DecodingFailure("Limits must have at least one of the fields defined", Nil))
    }

    "parse limits from json" in {
      val result = decode[Limits]("""{"min":10,"trailing":true}""")

      result mustBe Right(Limits(Some(10), None, None, None, true))
    }
  }
}
