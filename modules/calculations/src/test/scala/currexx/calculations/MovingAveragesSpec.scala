package currexx.calculations

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.math.BigDecimal.RoundingMode

class MovingAveragesSpec extends AnyWordSpec with Matchers {

  "A MovingAverages" should {
    "calculate Simple Moving Average" in {
      val sma9 = MovingAverages.simple(values, 9)

      sma9.take(6).map(rounded(4)) mustBe List(1.1296, 1.1298, 1.1306, 1.1337, 1.1379, 1.1426)
    }

    "calculate Exponential Moving Average" in {
      val ema12 = MovingAverages.exponential(values, 12)
      ema12.take(6).map(rounded(4)) mustBe List(1.1337, 1.1329, 1.133, 1.1357, 1.1389, 1.144)

      val ema26 = MovingAverages.exponential(values, 26)
      ema26.take(6).map(rounded(4)) mustBe List(1.145, 1.1455, 1.1466, 1.1489, 1.1514, 1.1546)
    }

    "calculate Moving Average Convergence/Divergence" in {
      val macd = MovingAverages.macd(values)
      macd.take(6).map(rounded(4)) mustBe List(-0.0113, -0.0127, -0.0136, -0.0132, -0.0124, -0.0106)
    }

    "calculate Weighted Moving Average" in {
      val wma = MovingAverages.weighted(values, 9)
      wma.take(6).map(rounded(4)) mustBe List(1.1273, 1.1256, 1.1252, 1.1284, 1.1324, 1.1387)
    }

    "calculate Hull Moving Average" in {
      val hma = MovingAverages.hull(values, 9)
      hma.take(6).map(rounded(4)) mustBe List(1.1224, 1.1154, 1.114, 1.1193, 1.1274, 1.1361)
    }

    "calculate Jurik Moving Average (simplified)" in {
      val jma = MovingAverages.jurikSimplified(values, 9, 50, 2)

      jma.take(6).map(rounded(4)) mustBe List(1.1264, 1.1202, 1.1172, 1.1201, 1.1262, 1.1359)
    }

    // 1.2999, 1.1214, 1.1155, 1.1169, 1,1223, 1.1341
  }

  def rounded(scale: Int)(num: Double): Double =
    BigDecimal(num).setScale(scale, RoundingMode.HALF_UP).toDouble

  // GBP-EUR - 2022-09-30
  val values: List[Double] = List(
    "1.13800",
    "1.13250",
    "1.11780",
    "1.11800",
    "1.11100",
    "1.11890",
    "1.14380",
    "1.14520",
    "1.14090",
    "1.13970",
    "1.13970",
    "1.14650",
    "1.15580",
    "1.15290",
    "1.15370",
    "1.15370",
    "1.15001",
    "1.15245",
    "1.16269",
    "1.15984",
    "1.15670",
    "1.15990",
    "1.15540",
    "1.16300",
    "1.17040",
    "1.17810",
    "1.18616",
    "1.18260",
    "1.18640",
    "1.18260",
    "1.17820",
    "1.18227",
    "1.18378",
    "1.18880",
    "1.18630",
    "1.18200",
    "1.18130",
    "1.18570",
    "1.18170",
    "1.18450",
    "1.18510",
    "1.18606",
    "1.19431",
    "1.19624",
    "1.19380",
    "1.19050",
    "1.19407",
    "1.19138",
    "1.18842",
    "1.17857",
    "1.17500",
    "1.17230",
    "1.17669",
    "1.17303",
    "1.17770",
    "1.17580",
    "1.17980",
    "1.18214",
    "1.18416",
    "1.18410",
    "1.18060",
    "1.18250",
    "1.16980",
    "1.16390",
    "1.16060",
    "1.15930",
    "1.16110",
    "1.16055",
    "1.15740",
    "1.15860",
    "1.16180",
    "1.16470",
    "1.16051",
    "1.16510",
    "1.16460",
    "1.16380",
    "1.16970",
    "1.16460",
    "1.15140",
    "1.16531",
    "1.17060",
    "1.17639",
    "1.16920",
    "1.17530",
    "1.17120",
    "1.16440",
    "1.16950",
    "1.17180",
    "1.17350",
    "1.17360",
    "1.17640",
    "1.17410",
    "1.17720",
    "1.16720",
    "1.17690",
    "1.18220",
    "1.17800",
    "1.17900",
    "1.18360",
    "1.18030"
  ).map(_.toDouble)
}
