package currexx.domain.signal

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

  "ValueTransformation codecs" should {
    "encode and decode all transformation types from json" in {
      val transformations: List[(ValueTransformation, String)] = List(
        ValueTransformation.Sequenced(List(ValueTransformation.SMA(10), ValueTransformation.EMA(20))) ->
          """{"sequence":[{"length":10,"kind":"sma"},{"length":20,"kind":"ema"}],"kind":"sequenced"}""",
        ValueTransformation.StandardDeviation(14) ->
          """{"length":14,"kind":"stdev"}""",
        ValueTransformation.Kalman(0.2, 0.05) ->
          """{"gain":0.2,"measurementNoise":0.05,"kind":"kalman"}""",
        ValueTransformation.KalmanVelocity(0.3, 0.04) ->
          """{"gain":0.3,"measurementNoise":0.04,"kind":"kalman-velocity"}""",
        ValueTransformation.ATR(14) ->
          """{"length":14,"kind":"atr"}""",
        ValueTransformation.RSX(14) ->
          """{"length":14,"kind":"rsx"}""",
        ValueTransformation.JRSX(14) ->
          """{"length":14,"kind":"jrsx"}""",
        ValueTransformation.WMA(10) ->
          """{"length":10,"kind":"wma"}""",
        ValueTransformation.SMA(10) ->
          """{"length":10,"kind":"sma"}""",
        ValueTransformation.EMA(10) ->
          """{"length":10,"kind":"ema"}""",
        ValueTransformation.HMA(10) ->
          """{"length":10,"kind":"hma"}""",
        ValueTransformation.NMA(14, 5, 1.5, MovingAverage.Exponential) ->
          """{"length":14,"signalLength":5,"lambda":1.5,"maCalc":"exponential","kind":"nma"}""",
        ValueTransformation.JMA(14, 50, 2) ->
          """{"length":14,"phase":50,"power":2,"kind":"jma"}""",
        ValueTransformation.STOCH(14) ->
          """{"length":14,"kind":"stoch"}""",
        ValueTransformation.ADX(14) ->
          """{"length":14,"kind":"adx"}""",
        ValueTransformation.WilliamsR(14) ->
          """{"length":14,"kind":"williams-r"}""",
        ValueTransformation.CCI(20) ->
          """{"length":20,"kind":"cci"}""",
        ValueTransformation.IchimokuKijunSen(26) ->
          """{"length":26,"kind":"ichimoku-kijun"}""",
        ValueTransformation.ParabolicSAR(0.02, 0.2, 0.02) ->
          """{"afStart":0.02,"afMax":0.2,"afStep":0.02,"kind":"parabolic-sar"}""",
        ValueTransformation.CMF(20) ->
          """{"length":20,"kind":"cmf"}"""
      )

      transformations.foreach { case (vt, json) =>
        vt.asJson.noSpaces mustBe json
        decode[ValueTransformation](json) mustBe Right(vt)
      }
    }

    "fail to decode unknown value transformation kind" in {
      decode[ValueTransformation]("""{"kind":"unknown"}""").isLeft mustBe true
    }
  }

  "Indicator codecs" should {
    "encode and decode indicator with single transformation from json" in {
      val indicator = Indicator.TrendChangeDetection(
        ValueSource.Close,
        ValueTransformation.NMA(9, 3, 4.2d, MovingAverage.Weighted)
      )

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
          ValueTransformation.Kalman(0.25, 0.05),
          ValueTransformation.HMA(6)
        )
      )

      val json =
        """{
          |"source":"close",
          |"transformation":{"sequence":[{"gain":0.25,"measurementNoise":0.05,"kind":"kalman"},{"length":6,"kind":"hma"}],"kind":"sequenced"},
          |"kind":"trend-change-detection"
          |}""".stripMargin.replaceAll("\n", "")

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }

    "encode and decode composite indicator from json" in {
      val inner     = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(10))
      val indicator = Indicator.Composite(cats.data.NonEmptyList.of(inner), CombinationLogic.All)
      val json      =
        """{"indicators":[{"source":"close","transformation":{"length":10,"kind":"sma"},"kind":"trend-change-detection"}],"combinator":"all","kind":"composite"}"""

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }

    "encode and decode volatility regime detection from json" in {
      val indicator = Indicator.VolatilityRegimeDetection(14, ValueTransformation.EMA(20))
      val json      = """{"atrLength":14,"smoothingType":{"length":20,"kind":"ema"},"kind":"volatility-regime-detection"}"""

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }

    "encode and decode threshold crossing from json" in {
      val indicator = Indicator.ThresholdCrossing(ValueSource.Close, ValueTransformation.RSX(14), 70.0, 30.0)
      val json      =
        """{"source":"close","transformation":{"length":14,"kind":"rsx"},"upperBoundary":70.0,"lowerBoundary":30.0,"kind":"threshold-crossing"}"""

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }

    "encode and decode lines crossing from json" in {
      val indicator = Indicator.LinesCrossing(ValueSource.Close, ValueTransformation.SMA(50), ValueTransformation.SMA(20))
      val json      =
        """{"source":"close","line1Transformation":{"length":50,"kind":"sma"},"line2Transformation":{"length":20,"kind":"sma"},"kind":"lines-crossing"}"""

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }

    "encode and decode keltner channel from json" in {
      val indicator = Indicator.KeltnerChannel(ValueSource.Close, ValueTransformation.EMA(20), 10, 2.0)
      val json      =
        """{"source":"close","middleBand":{"length":20,"kind":"ema"},"atrLength":10,"atrMultiplier":2.0,"kind":"keltner-channel"}"""

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }

    "encode and decode bollinger bands from json" in {
      val indicator = Indicator.BollingerBands(ValueSource.Close, ValueTransformation.SMA(20), 20, 2.0)
      val json      =
        """{"source":"close","middleBand":{"length":20,"kind":"sma"},"stdDevLength":20,"stdDevMultiplier":2.0,"kind":"bollinger-bands"}"""

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }

    "encode and decode value tracking from json" in {
      val indicator = Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, ValueTransformation.RSX(14))
      val json      = """{"role":"momentum","source":"close","transformation":{"length":14,"kind":"rsx"},"kind":"value-tracking"}"""

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }

    "encode and decode price line crossing from json" in {
      val indicator = Indicator.PriceLineCrossing(ValueSource.Close, ValueRole.ChannelMiddleBand, ValueTransformation.EMA(20))
      val json      =
        """{"source":"close","role":"channel-middle-band","transformation":{"length":20,"kind":"ema"},"kind":"price-line-crossing"}"""

      indicator.asJson.noSpaces mustBe json
      decode[Indicator](json) mustBe Right(indicator)
    }

    "fail to decode unknown indicator kind" in {
      decode[Indicator]("""{"kind":"unknown"}""").isLeft mustBe true
    }
  }
}
