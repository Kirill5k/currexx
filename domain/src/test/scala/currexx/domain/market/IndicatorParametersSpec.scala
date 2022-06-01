package currexx.domain.market

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.circe.syntax.*
import io.circe.parser.*

class IndicatorParametersSpec extends AnyWordSpec with Matchers{

  "An IndicatorParameters codecs" should {
    "decode indicator parameters from json" in {
      val params: IndicatorParameters = IndicatorParameters.MACD()

      params.asJson.noSpaces mustBe """{"fastLength":12,"slowLength":26,"signalSmoothing":9,"indicator":"macd"}"""
    }

    "encode indicator parameters to json" in {
      val json = """{"indicator":"macd","fastLength":12,"slowLength":26,"signalSmoothing":9}"""

      decode[IndicatorParameters](json) mustBe Right(IndicatorParameters.MACD())
    }
  }
}
