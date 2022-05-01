package currexx.calculations

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.math.BigDecimal.RoundingMode

class MovingAverageCalculatorSpec extends AnyWordSpec with Matchers {

  "A MovingAverageCalculator" should {
    "calculate Simple Moving Average" in {
      val sma9 = MovingAverageCalculator.sma(values, 9)

      sma9.take(5).map(rounded(4)) mustBe List(1.2756, 1.2805, 1.2872, 1.2931, 1.2991)
    }

    "calculate Exponential Moving Average" in {
      val ema12 = MovingAverageCalculator.ema(values, 12)
      ema12.take(5).map(rounded(4)) mustBe List(1.2761, 1.2795, 1.2857, 1.2914, 1.2977)

      val ema26 = MovingAverageCalculator.ema(values, 26)
      ema26.take(5).map(rounded(4)) mustBe List(1.2918, 1.2946, 1.2985, 1.3020, 1.3056)
    }

    "calculate Moving Average Convergence/Divergence" in {
      val macd = MovingAverageCalculator.macd(values)
      macd.take(5).map(rounded(4)) mustBe List(-0.0157, -0.0151, -0.0128, -0.0106, -0.0079)
    }

    "calculate Weighted Moving Average" in {
      val wma = MovingAverageCalculator.wma(values, 9)
      wma.take(5).map(rounded(4)) mustBe List(1.2650, 1.2697, 1.2780, 1.2859, 1.2943)
    }
  }

  def rounded(scale: Int)(num: BigDecimal): BigDecimal =
    num.setScale(scale, RoundingMode.HALF_UP)

  val values = List(
    "1.25710",
    "1.24554",
    "1.25396",
    "1.25711",
    "1.27420",
    "1.28370",
    "1.30320",
    "1.30615",
    "1.29980",
    "1.30078",
    "1.30580",
    "1.30707",
    "1.31147",
    "1.30010",
    "1.30294",
    "1.30310",
    "1.30740",
    "1.30650",
    "1.30712",
    "1.31140",
    "1.31120",
    "1.31371",
    "1.31332",
    "1.30970",
    "1.30921",
    "1.31880",
    "1.31830",
    "1.32025",
    "1.32583",
    "1.31622",
    "1.31800",
    "1.31455",
    "1.31420",
    "1.30355",
    "1.30012",
    "1.30343",
    "1.30850",
    "1.31757",
    "1.31012",
    "1.31033",
    "1.32287",
    "1.33448",
    "1.34052",
    "1.33250",
    "1.34161",
    "1.34053",
    "1.33749",
    "1.35430",
    "1.35801",
    "1.35973",
    "1.35856",
    "1.36130",
    "1.35860",
    "1.35341",
    "1.35233",
    "1.35584",
    "1.35560",
    "1.35340",
    "1.35420",
    "1.35357",
    "1.35269",
    "1.35947",
    "1.35757",
    "1.35205",
    "1.34471",
    "1.33980",
    "1.33819",
    "1.34622",
    "1.35015",
    "1.34870",
    "1.35504",
    "1.35992",
    "1.36120",
    "1.35960",
    "1.36460",
    "1.36762",
    "1.37050",
    "1.37000",
    "1.36357",
    "1.35740",
    "1.35860",
    "1.35290",
    "1.35541",
    "1.35290",
    "1.34790",
    "1.35177",
    "1.34980",
    "1.34890",
    "1.34340",
    "1.34390",
    "1.33849",
    "1.34055",
    "1.33481",
    "1.32630",
    "1.32060",
    "1.32380",
    "1.33230",
    "1.32603",
    "1.32310",
    "1.32131"
  ).map(BigDecimal.apply)
}
