package currexx.backtest.optimizer.strategy

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class ParameterDomainSpec extends AnyWordSpec with Matchers {
  import RuleParameterBounds.DoubleRange

  "Numeric rule grids" should {
    "use the represented grid for both membership and snapping" in {
      given Random = new Random(1) {
        override def nextGaussian(): Double = 0.0
      }
      val domain = ParameterDomain.real(DoubleRange(1.0, 1.000000000000003, 3e-16)).toOption.get
      // Index 4 represents this Double, but its shortest decimal text would round to decimal index 3.
      val value = 1.000000000000001
      domain.contains(value) mustBe true
      domain.mutate(value, 1.0) mustBe value
      domain.contains(java.lang.Math.nextUp(1.0)) mustBe true
      domain.contains(1.0000000000000004) mustBe false
    }

    "round ordinary decimal midpoints towards the upper grid point" in {
      given Random = new Random(1) {
        override def nextGaussian(): Double = 1.25
        override def nextDouble(): Double   = 0.5
      }
      val domain = ParameterDomain.real(DoubleRange(0, 4, 1)).toOption.get
      domain.mutate(1, 1.0) mustBe 2.0
      domain.cross(1, 2) mustBe 2.0
      val decimal = ParameterDomain.real(DoubleRange(0, 1, 0.1)).toOption.get
      decimal.cross(0.2, 0.3) mustBe 0.3
    }

    "keep exact membership without accepting nearby off-grid numbers" in {
      val domain = ParameterDomain.real(DoubleRange(1, 8, 0.25)).toOption.get
      domain.contains(4.0) mustBe true
      domain.contains(4.13) mustBe false
      domain.contains(java.lang.Math.nextUp(4.0)) mustBe false
      domain.contains(Double.NaN) mustBe false
      domain.contains(Double.PositiveInfinity) mustBe false
    }
  }
}
