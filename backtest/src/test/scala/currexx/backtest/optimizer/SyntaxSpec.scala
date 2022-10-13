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
  }
}
