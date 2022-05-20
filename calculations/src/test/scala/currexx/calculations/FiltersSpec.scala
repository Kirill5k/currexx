package currexx.calculations

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FiltersSpec extends AnyWordSpec with Matchers {

  "A Filters" should {

    "use kalman filter for removing noise from measurements of moving objects" in {
      val result = Filters.ghKalman(
        values = List(30110D, 30265D, 30740D, 30750D, 31135D, 31015D, 31180D, 31610D, 31960D, 31865D).reverse,
        alpha = 0.2,
        beta = 0.1,
        initialGuess = 30000,
        initialVelocity = 40,
        time = 5
      )

      result mustBe List(
        31952.873160384,
        31764.32870592,
        31529.3570496,
        31333.222848,
        31176.402240000003,
        31001.451200000003,
        30769.456000000002,
        30573.280000000002,
        30351.4,
        30182.0
      )
    }

    "use kalman filter for removing noise from measurements of accelerating objects" in {
      val result = Filters.ghkKalman(
        values = List(30160D, 30365D, 30890D, 31050D, 31785D, 32215D, 33130D, 34510D, 36010D, 37265D).reverse,
        alpha = 0.5,
        beta = 0.4,
        gamma = 0.1,
        initialGuess = 30000,
        initialVelocity = 50,
        initialAcceleration = 0,
        time = 5
      )

      result mustBe List(
        37465.405750000005,
        35822.889500000005,
        34250.6725,
        33032.54,
        32201.725,
        31591.05,
        31038.75,
        30721.0,
        30387.5,
        30205.0
      )
    }
  }
}
