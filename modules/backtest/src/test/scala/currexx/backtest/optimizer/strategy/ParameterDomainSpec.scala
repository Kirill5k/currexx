package currexx.backtest.optimizer.strategy

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.*
import scala.util.Random

class ParameterDomainSpec extends AnyWordSpec with Matchers {
  import RuleParameterBounds.{DoubleRange, DurationRange}

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

    "keep every represented decimal grid point valid through sampling, mutation and crossover" in {
      var index    = 0
      given Random = new Random(1):
        override def nextInt(bound: Int): Int = { val current = index; index += 1; current }
        override def nextGaussian(): Double   = 1.0
        override def nextDouble(): Double     = 0.5
      val domain    = ParameterDomain.real(DoubleRange(1.0, 1.000000000000003, 3e-16)).toOption.get
      val samples   = List.fill(11)(domain.sample)
      val mutations = samples.map(domain.mutate(_, 1.0))
      val children  = samples.zip(samples.reverse).map(domain.cross(_, _))
      samples.distinct.size must be > 1
      (samples ++ mutations ++ children).foreach(value => domain.contains(value) mustBe true)
    }

    "scale nearby Gaussian changes by the requested radius" in {
      given Random = new Random(1):
        override def nextGaussian(): Double = 1.0
      val domain = ParameterDomain.real(DoubleRange(0, 1, 0.01)).toOption.get
      List(1.0, 3.0, 6.0).map(domain.nearby(0.5, _)) mustBe List(0.6, 0.8, 1.0)
    }
  }

  "Duration rule grids" should {
    "sample and vary exact nanosecond indices above Double's integer precision" in {
      var index    = 0
      given Random = new Random(1):
        override def nextInt(bound: Int): Int = { val current = index; index += 1; current }
        override def nextGaussian(): Double   = 1.0
        override def nextDouble(): Double     = 0.5
      val offset  = (1L << 53) + 1
      val domain  = ParameterDomain.duration(DurationRange(offset.nanos, (offset + 10).nanos, 1.nano)).toOption.get
      val samples = List.fill(11)(domain.sample)
      samples.map(_.toNanos) mustBe (0L to 10L).map(offset + _).toList
      samples.map(domain.mutate(_, 1.0)) mustBe samples.tail :+ samples.last
      samples.init.zip(samples.tail).map(domain.cross(_, _)) mustBe samples.tail
      samples.foreach(value => domain.contains(value) mustBe true)
      domain.contains((offset - 1).nanos) mustBe false
      domain.distance(samples.head, samples.last) mustBe 1.0
    }
  }
}
