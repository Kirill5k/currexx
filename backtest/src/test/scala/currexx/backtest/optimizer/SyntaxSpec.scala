package currexx.backtest.optimizer

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers
import currexx.backtest.optimizer.syntax.*

class SyntaxSpec extends AnyWordSpec with Matchers {

  "Syntax" should {
    "provide extension method for converting binary array to int" in {
      val array = "11100111100".map(_.toString.toInt).toArray

      array.toInt mustBe 1852
    }

    "provide extension method for converting int to binary array" in {
      256.toBinaryArray(512).mkString("") mustBe "0100000000"
      1852.toBinaryArray(2000).mkString("") mustBe "11100111100"
    }

    "provide extension method for calculating median" in {
      List(1).map(BigDecimal(_)).median mustBe BigDecimal(1)
      List(1, 2).map(BigDecimal(_)).median mustBe BigDecimal(1.5)
      List(1, 2, 3, 4, 5, 6, 7, 8).map(BigDecimal(_)).median mustBe BigDecimal(4.5)
      List(7, 5, 3, 2, 1, 4, 8, 6).map(BigDecimal(_)).median mustBe BigDecimal(4.5)
      List(1, 2, 3, 4, 5, 6, 7, 8, 9).map(BigDecimal(_)).median mustBe BigDecimal(5)
    }
  }
}
