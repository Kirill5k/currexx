package currexx.calculations

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FiltersSpec extends AnyWordSpec with Matchers {

  "A Filters" should {

    "use kalman filter for removing noise from measurements" in {
      val result = Filters.kalman(
        values = List(50.45, 50.967, 51.6, 52.106, 52.492, 52.819, 53.433, 54.007, 54.523, 54.99).reverse,
        gain = 0.6
      )

      result mustBe List(55.0094967026825, 54.513152000153546, 53.960265894839225, 53.3854609036031, 52.87666588945132, 52.54188257632462,
        52.12559536215797, 51.59852146088533, 50.96648359386705, 50.45)
    }

    "initialize price and velocity from the oldest measurement" in {
      Filters.kalman(Array.emptyDoubleArray, 0.6, 1.0).toList mustBe Nil
      Filters.kalmanVelocity(Array.emptyDoubleArray, 0.6, 1.0).toList mustBe Nil
      Filters.kalman(Array(3.0), 0.6, 1.0).toList mustBe List(3.0)
      Filters.kalmanVelocity(Array(3.0), 0.6, 1.0).toList mustBe List(0.0)
      Filters.kalman(Array.fill(100)(3.0), 0.6, 1.0).toList mustBe List.fill(100)(3.0)
      Filters.kalmanVelocity(Array.fill(100)(3.0), 0.6, 1.0).toList mustBe List.fill(100)(0.0)
    }

    "apply measurement noise to both price and velocity estimates" in {
      Filters.kalman(Array(2.0, 0.0), 0.0, 1000.0).toList mustBe List(1.0, 0.0)
      Filters.kalmanVelocity(Array(2.0, 0.0), 0.0, 1000.0).toList mustBe List(0.5, 0.0)
    }

    "return independent price and velocity arrays without changing measurements" in {
      val input        = Array(4.0, 2.0, 0.0)
      val price        = Filters.kalman(input, 0.0, 0.0)
      val velocity     = Filters.kalmanVelocity(input, 0.0, 0.0)
      val nextPrice    = Filters.kalman(input, 0.0, 0.0)
      val nextVelocity = Filters.kalmanVelocity(input, 0.0, 0.0)
      price.toList mustBe List(4.0, 2.0, 0.0)
      velocity.toList mustBe List(2.0, 1.0, 0.0)
      price(0) = -1.0
      velocity(0) = -1.0
      input.toList mustBe List(4.0, 2.0, 0.0)
      nextPrice.toList mustBe List(4.0, 2.0, 0.0)
      nextVelocity.toList mustBe List(2.0, 1.0, 0.0)
    }

    "propagate nonfinite measurements after initialization" in {
      for (measurement <- List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)) {
        val prices     = Filters.kalman(Array(measurement, 2.0, 1.0), 0.6, 1.0)
        val velocities = Filters.kalmanVelocity(Array(measurement, 2.0, 1.0), 0.6, 1.0)
        if (measurement.isNaN) {
          prices.head.isNaN mustBe true
          velocities.head.isNaN mustBe true
        } else {
          prices.head mustBe measurement
          velocities.head mustBe measurement
        }
        prices.last mustBe 1.0
        velocities.last mustBe 0.0
      }
      succeed
    }
  }
}
