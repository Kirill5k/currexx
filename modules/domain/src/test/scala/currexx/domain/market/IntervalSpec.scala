package currexx.domain.market

import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IntervalSpec extends AnyWordSpec with Matchers {
  "An Interval" should {
    "have correct duration for M1" in {
      Interval.M1.toDuration.toSeconds mustEqual 60
    }

    "have correct duration for M5" in {
      Interval.M5.toDuration.toSeconds mustEqual 300
    }

    "have correct duration for M15" in {
      Interval.M15.toDuration.toSeconds mustEqual 900
    }

    "have correct duration for M30" in {
      Interval.M30.toDuration.toSeconds mustEqual 1800
    }

    "have correct duration for H1" in {
      Interval.H1.toDuration.toSeconds mustEqual 3600
    }

    "have correct duration for D1" in {
      Interval.D1.toDuration.toSeconds mustEqual 86400
    }

    "decode order from json" in {
      Interval.H1.asJson.noSpaces mustBe """"H1""""
    }
  }
}
