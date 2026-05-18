package currexx.calculations

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.math.BigDecimal.RoundingMode

class MomentumOscillatorsSpec extends AnyWordSpec with Matchers {

  "A MomentumOscillators" when {
    "jurikRelativeStrengthIndex" should {
      "calculate Relative Strength Index" in {
        val rsx = MomentumOscillators.jurikRelativeStrengthIndex(values.map(_._4), 14)

        rsx.take(5).map(rounded(4)) mustBe List(58.6569, 59.6337, 59.9322, 59.6654, 58.2619)
      }
    }

    "relativeStrengthIndex" should {
      "calculate Relative Strength Index" in {
        val rsi = MomentumOscillators.relativeStrengthIndex(values.map(_._4), 14)

        rsi.take(5).map(rounded(4)) mustBe List(49.206, 51.0793, 51.3351, 54.7208, 52.5356)
      }
    }

    "stochastic" should {
      "calculate stochastic oscillator values (smooth k and smooth d)" in {
        val stoch = MomentumOscillators.stochastic(
          closings = values.map(_._4),
          highs = values.map(_._2),
          lows = values.map(_._3),
          length = 14
        )

        stoch.take(5).map(rounded(4)) mustBe List(45.7173, 53.731, 67.1259, 81.6228, 80.055)
      }
    }
    "averageDirectionalIndex" should {
      "calculate ADX values" in {
        val adx = MomentumOscillators.averageDirectionalIndex(
          closings = values.map(_._4),
          highs = values.map(_._2),
          lows = values.map(_._3),
          length = 14
        )

        adx.size mustBe values.size
        // First 14 values are warm-up (0.0), then ADX starts
        adx.takeRight(14) mustBe List.fill(14)(0.0)
        // ADX should be between 0 and 100
        adx.foreach(v => assert(v >= 0.0 && v <= 100.0, s"ADX value $v out of range"))
        // Verify some computed values
        adx.take(5).map(rounded(4)) mustBe List(26.523, 27.5621, 29.0376, 30.6266, 32.8747)
      }

      "return zeros for insufficient data" in {
        val adx = MomentumOscillators.averageDirectionalIndex(List(1.0), List(1.5), List(0.5), 14)
        adx mustBe List(0.0)
      }
    }

    "williamsR" should {
      "calculate Williams %R values" in {
        val wr = MomentumOscillators.williamsR(
          closings = values.map(_._4),
          highs = values.map(_._2),
          lows = values.map(_._3),
          length = 14
        )

        wr.size mustBe values.size
        // Williams %R should be between -100 and 0
        wr.drop(14).foreach(v => assert(v >= -100.0 && v <= 0.0, s"Williams %%R value $v out of range"))
        wr.take(5).map(rounded(4)) mustBe List(-54.2827, -46.269, -32.8741, -18.3772, -19.945)
      }
    }

    "commodityChannelIndex" should {
      "calculate CCI values" in {
        val cci = MomentumOscillators.commodityChannelIndex(
          closings = values.map(_._4),
          highs = values.map(_._2),
          lows = values.map(_._3),
          length = 20
        )

        cci.size mustBe values.size
        // CCI is unbounded but typically between -300 and 300
        cci.take(5).map(rounded(4)) mustBe List(42.4933, 63.6702, 78.9702, 127.584, 107.1128)
      }
    }

    "ichimokuKijunSen" should {
      "calculate Kijun-Sen (base line) values" in {
        val kijun = MomentumOscillators.ichimokuKijunSen(
          highs = values.map(_._2),
          lows = values.map(_._3),
          length = 26
        )

        kijun.size mustBe values.size
        // Kijun-Sen is (highest high + lowest low) / 2 over 26 periods
        kijun.take(5).map(rounded(5)) mustBe List(1.12338, 1.12338, 1.12338, 1.12338, 1.12131)
      }
    }

    "parabolicSAR" should {
      "calculate Parabolic SAR values" in {
        val sar = MomentumOscillators.parabolicSAR(
          highs = values.map(_._2),
          lows = values.map(_._3),
          afStart = 0.02,
          afMax = 0.2,
          afStep = 0.02
        )

        sar.size mustBe values.size
        // SAR should be within the price range (roughly)
        val allHighs = values.map(_._2)
        val allLows  = values.map(_._3)
        val maxHigh  = allHighs.max
        val minLow   = allLows.min
        sar.foreach(v => assert(v >= minLow * 0.95 && v <= maxHigh * 1.05, s"SAR value $v out of expected range"))
      }

      "return zeros for insufficient data" in {
        val sar = MomentumOscillators.parabolicSAR(List(1.5), List(0.5), 0.02, 0.2, 0.02)
        sar mustBe List(0.0)
      }
    }

    "chaikinMoneyFlow" should {
      "calculate CMF values" in {
        val volumes = List.fill(values.size)(1000.0) // uniform volume for predictable results
        val cmf = MomentumOscillators.chaikinMoneyFlow(
          closings = values.map(_._4),
          highs = values.map(_._2),
          lows = values.map(_._3),
          volumes = volumes,
          length = 20
        )

        cmf.size mustBe values.size
        // CMF should be between -1 and 1
        cmf.drop(20).foreach(v => assert(v >= -1.0 && v <= 1.0, s"CMF value $v out of range"))
      }

      "return zero when volume is zero" in {
        val cmf = MomentumOscillators.chaikinMoneyFlow(
          closings = values.take(20).map(_._4),
          highs = values.take(20).map(_._2),
          lows = values.take(20).map(_._3),
          volumes = List.fill(20)(0.0),
          length = 20
        )
        cmf.head mustBe 0.0
      }
    }
  }

  def rounded(scale: Int)(num: Double): Double =
    BigDecimal(num).setScale(scale, RoundingMode.HALF_UP).toDouble

  // OPEN, HIGH, LOW, CLOSE
  val values: List[(Double, Double, Double, Double)] = List(
    ("1.14771", "1.14854", "1.14150", "1.14470"),
    ("1.14816", "1.15168", "1.14520", "1.14775"),
    ("1.15413", "1.15742", "1.14510", "1.14818"),
    ("1.15336", "1.16536", "1.15080", "1.15390"),
    ("1.15861", "1.15894", "1.14710", "1.14960"),
    ("1.14312", "1.16121", "1.14020", "1.15850"),
    ("1.13066", "1.14539", "1.12730", "1.14304"),
    ("1.13961", "1.14599", "1.12820", "1.13050"),
    ("1.13850", "1.14277", "1.13400", "1.13910"),
    ("1.14036", "1.14446", "1.13500", "1.13820"),
    ("1.14420", "1.14685", "1.13429", "1.13960"),
    ("1.14863", "1.15037", "1.13910", "1.14520"),
    ("1.15150", "1.15593", "1.14320", "1.14850"),
    ("1.14160", "1.15315", "1.13190", "1.15180"),
    ("1.13239", "1.14228", "1.12940", "1.13830"),
    ("1.11788", "1.13363", "1.11310", "1.13250"),
    ("1.11809", "1.12195", "1.10300", "1.11780"),
    ("1.11150", "1.12384", "1.11020", "1.11800"),
    ("1.11740", "1.12780", "1.08140", "1.11100"),
    ("1.14398", "1.14778", "1.11860", "1.11890"),
    ("1.14533", "1.15007", "1.14120", "1.14380"),
    ("1.14108", "1.14786", "1.14000", "1.14520"),
    ("1.14003", "1.14611", "1.13810", "1.14090"),
    ("1.14000", "1.14294", "1.13770", "1.13970"),
    ("1.14685", "1.14724", "1.13800", "1.13970"),
    ("1.15603", "1.15699", "1.14630", "1.14650"),
    ("1.15274", "1.15898", "1.15120", "1.15580"),
    ("1.15387", "1.15628", "1.15040", "1.15290"),
    ("1.15244", "1.15606", "1.14620", "1.15370"),
    ("1.15034", "1.15469", "1.14810", "1.15370"),
    ("1.15250", "1.15536", "1.14740", "1.15001"),
    ("1.16262", "1.16324", "1.15080", "1.15245"),
    ("1.15958", "1.16724", "1.15930", "1.16269"),
    ("1.15705", "1.16053", "1.15536", "1.15984"),
    ("1.16046", "1.16078", "1.15210", "1.15670"),
    ("1.15552", "1.16094", "1.15300", "1.15990"),
    ("1.16313", "1.16650", "1.15516", "1.15540"),
    ("1.17044", "1.17329", "1.16180", "1.16300"),
    ("1.17760", "1.17760", "1.16920", "1.17040"),
    ("1.18610", "1.18655", "1.17603", "1.17810"),
    ("1.18295", "1.18706", "1.18140", "1.18616"),
    ("1.18678", "1.18925", "1.18220", "1.18260"),
    ("1.18283", "1.18816", "1.18270", "1.18640"),
    ("1.17800", "1.18545", "1.17700", "1.18260"),
    ("1.18232", "1.18289", "1.17440", "1.17820"),
    ("1.18350", "1.18684", "1.18110", "1.18227"),
    ("1.18891", "1.19155", "1.18290", "1.18378"),
    ("1.18613", "1.18961", "1.18440", "1.18880"),
    ("1.18220", "1.18768", "1.18030", "1.18630"),
    ("1.18169", "1.18383", "1.17710", "1.18200"),
    ("1.18563", "1.18623", "1.18010", "1.18130"),
    ("1.18195", "1.18778", "1.18030", "1.18570"),
    ("1.18496", "1.18563", "1.18130", "1.18170"),
    ("1.18550", "1.18896", "1.18360", "1.18450"),
    ("1.18608", "1.18884", "1.18250", "1.18510"),
    ("1.19432", "1.19647", "1.18490", "1.18606"),
    ("1.19622", "1.19856", "1.19350", "1.19431"),
    ("1.19424", "1.19890", "1.19020", "1.19624"),
    ("1.19070", "1.19678", "1.19000", "1.19380"),
    ("1.19382", "1.19470", "1.18810", "1.19050"),
    ("1.19175", "1.19804", "1.18950", "1.19407"),
    ("1.18843", "1.19378", "1.18640", "1.19138"),
    ("1.17840", "1.18969", "1.17720", "1.18842"),
    ("1.17490", "1.17977", "1.17260", "1.17857"),
    ("1.17267", "1.17820", "1.17230", "1.17500"),
    ("1.17650", "1.17680", "1.16440", "1.17230"),
    ("1.17306", "1.17735", "1.17040", "1.17669"),
    ("1.17749", "1.17952", "1.17060", "1.17303"),
    ("1.17590", "1.18224", "1.17570", "1.17770"),
    ("1.18000", "1.18132", "1.17440", "1.17580"),
    ("1.18194", "1.18462", "1.17670", "1.17980"),
    ("1.18419", "1.18991", "1.18060", "1.18214"),
    ("1.18431", "1.18570", "1.17830", "1.18416"),
    ("1.18239", "1.18451", "1.17770", "1.18410"),
    ("1.18279", "1.18431", "1.17940", "1.18060"),
    ("1.17007", "1.18399", "1.16880", "1.18250"),
    ("1.16398", "1.17189", "1.16220", "1.16980"),
    ("1.16068", "1.17039", "1.15920", "1.16390"),
    ("1.16061", "1.16336", "1.15870", "1.16060"),
    ("1.16114", "1.16152", "1.15180", "1.15930"),
    ("1.16048", "1.16922", "1.15960", "1.16110"),
    ("1.15772", "1.16216", "1.15410", "1.16055"),
    ("1.15871", "1.16119", "1.15610", "1.15740"),
    ("1.16246", "1.16565", "1.15810", "1.15860"),
    ("1.16451", "1.16786", "1.16180", "1.16180"),
    ("1.16037", "1.16628", "1.15680", "1.16470"),
    ("1.16500", "1.16610", "1.15900", "1.16051"),
    ("1.16493", "1.16700", "1.16100", "1.16510"),
    ("1.16490", "1.16645", "1.16090", "1.16460"),
    ("1.16980", "1.17317", "1.16310", "1.16380"),
    ("1.16478", "1.17467", "1.15839", "1.16970"),
    ("1.15136", "1.16733", "1.14620", "1.16460"),
    ("1.16539", "1.16887", "1.14720", "1.15140"),
    ("1.17070", "1.17204", "1.16330", "1.16531"),
    ("1.17660", "1.17761", "1.16930", "1.17060"),
    ("1.16947", "1.17828", "1.16340", "1.17639"),
    ("1.17550", "1.17678", "1.16740", "1.16920"),
    ("1.17126", "1.17744", "1.16490", "1.17530"),
    ("1.16490", "1.17286", "1.16370", "1.17120"),
    ("1.16949", "1.17067", "1.16440", "1.16440")
  ).map { case (o, h, l, c) => (o.toDouble, h.toDouble, l.toDouble, c.toDouble) }
}
