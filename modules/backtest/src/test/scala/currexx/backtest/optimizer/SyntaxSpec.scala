package currexx.backtest.optimizer

import currexx.backtest.syntax.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

class SyntaxSpec extends AnyWordSpec with Matchers {

  "Syntax" should {
    "provide extension method for calculating median" in {
      List(1).map(BigDecimal(_)).median mustBe BigDecimal(1)
      List(1, 2).map(BigDecimal(_)).median mustBe BigDecimal(1.5)
      List(1, 2, 3, 4, 5, 6, 7, 8).map(BigDecimal(_)).median mustBe BigDecimal(4.5)
      List(7, 5, 3, 2, 1, 4, 8, 6).map(BigDecimal(_)).median mustBe BigDecimal(4.5)
      List(1, 2, 3, 4, 5, 6, 7, 8, 9).map(BigDecimal(_)).median mustBe BigDecimal(5)
    }
  }
}
