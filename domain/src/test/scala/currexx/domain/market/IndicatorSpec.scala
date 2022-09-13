package currexx.domain.market

import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IndicatorSpec extends AnyWordSpec with Matchers {

  "MovingAverage codecs" should {
    "encode and decode MA from json" in {
      val ma = MovingAverage.Weighted

      val json = """"weighted""""

      ma.asJson.noSpaces mustBe json
      decode[MovingAverage](json) mustBe Right(ma)
    }
  }

  "Indicator codecs" should {
    "encode and decode indicator with single transformation from json" in {
      val indicator = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.NMA(9, 3, 4.2d, MovingAverage.Weighted))

      val json =
        """{
          |"source":"close",
          |"transformation":{"length":9,"signalLength":3,"lambda":4.2,"maCalc":"weighted","kind":"nma"},
          |"kind":"trend-change-detection"
          |}""".stripMargin.replaceAll("\n", "")

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }

    "encode and decode indicator with sequenced transformation from json" in {
      val indicator = Indicator.TrendChangeDetection(
        source = ValueSource.Close,
        transformation = ValueTransformation.sequenced(
          ValueTransformation.Kalman(0.25),
          ValueTransformation.HMA(6)
        )
      )

      val json =
        """{
          |"source":"close",
          |"transformation":{"sequence":[{"gain":0.25,"kind":"kalman"},{"length":6,"kind":"hma"}],"kind":"sequenced"},
          |"kind":"trend-change-detection"
          |}""".stripMargin.replaceAll("\n", "")

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }
  }
}
