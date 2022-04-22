package currexx.calculations

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.math.BigDecimal.RoundingMode

class MomentumOscillatorCalculatorSpec extends AnyWordSpec with Matchers {

  "A MovingAverageCalculator" should {
    "calculate Relative Strength Index" in {
      val rsi = MomentumOscillatorCalculator.rsi(values, 14)

      rsi.take(5).map(rounded(4)) mustBe List(
        BigDecimal("32.1880"),
        BigDecimal("42.5122"),
        BigDecimal("45.0495"),
        BigDecimal("37.6063"),
        BigDecimal("38.3507")
      )
    }
  }

  def rounded(scale: Int)(num: BigDecimal): BigDecimal =
    num.setScale(scale, RoundingMode.HALF_UP)

  val values = List(
    BigDecimal("1.28760"),
    BigDecimal("1.30320"),
    BigDecimal("1.30615"),
    BigDecimal("1.29980"),
    BigDecimal("1.30078"),
    BigDecimal("1.30580"),
    BigDecimal("1.30707"),
    BigDecimal("1.31147"),
    BigDecimal("1.30010"),
    BigDecimal("1.30294"),
    BigDecimal("1.30310"),
    BigDecimal("1.30740"),
    BigDecimal("1.30650"),
    BigDecimal("1.30712"),
    BigDecimal("1.31140"),
    BigDecimal("1.31120"),
    BigDecimal("1.31371"),
    BigDecimal("1.31332"),
    BigDecimal("1.30970"),
    BigDecimal("1.30921"),
    BigDecimal("1.31880"),
    BigDecimal("1.31830"),
    BigDecimal("1.32025"),
    BigDecimal("1.32583"),
    BigDecimal("1.31622"),
    BigDecimal("1.31800"),
    BigDecimal("1.31455"),
    BigDecimal("1.31420"),
    BigDecimal("1.30355"),
    BigDecimal("1.30012"),
    BigDecimal("1.30343"),
    BigDecimal("1.30850"),
    BigDecimal("1.31757"),
    BigDecimal("1.31012"),
    BigDecimal("1.31033"),
    BigDecimal("1.32287"),
    BigDecimal("1.33448"),
    BigDecimal("1.34052"),
    BigDecimal("1.33250"),
    BigDecimal("1.34161"),
    BigDecimal("1.34053"),
    BigDecimal("1.33749"),
    BigDecimal("1.35430"),
    BigDecimal("1.35801"),
    BigDecimal("1.35973"),
    BigDecimal("1.35856"),
    BigDecimal("1.36130"),
    BigDecimal("1.35860"),
    BigDecimal("1.35341"),
    BigDecimal("1.35233"),
    BigDecimal("1.35584"),
    BigDecimal("1.35560"),
    BigDecimal("1.35340"),
    BigDecimal("1.35420"),
    BigDecimal("1.35357"),
    BigDecimal("1.35269"),
    BigDecimal("1.35947"),
    BigDecimal("1.35757"),
    BigDecimal("1.35205"),
    BigDecimal("1.34471"),
    BigDecimal("1.33980"),
    BigDecimal("1.33819"),
    BigDecimal("1.34622"),
    BigDecimal("1.35015"),
    BigDecimal("1.34870"),
    BigDecimal("1.35504"),
    BigDecimal("1.35992"),
    BigDecimal("1.36120"),
    BigDecimal("1.35960"),
    BigDecimal("1.36460"),
    BigDecimal("1.36762"),
    BigDecimal("1.37050"),
    BigDecimal("1.37000"),
    BigDecimal("1.36357"),
    BigDecimal("1.35740"),
    BigDecimal("1.35860"),
    BigDecimal("1.35290"),
    BigDecimal("1.35541"),
    BigDecimal("1.35290"),
    BigDecimal("1.34790"),
    BigDecimal("1.35177"),
    BigDecimal("1.34980"),
    BigDecimal("1.34890"),
    BigDecimal("1.34340"),
    BigDecimal("1.34390"),
    BigDecimal("1.33849"),
    BigDecimal("1.34055"),
    BigDecimal("1.33481"),
    BigDecimal("1.32630"),
    BigDecimal("1.32060"),
    BigDecimal("1.32380"),
    BigDecimal("1.33230"),
    BigDecimal("1.32603"),
    BigDecimal("1.32310"),
    BigDecimal("1.32131"),
    BigDecimal("1.32684"),
    BigDecimal("1.32186"),
    BigDecimal("1.32019"),
    BigDecimal("1.32423"),
    BigDecimal("1.32620")
  )
}
