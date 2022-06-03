package currexx.calculations

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FiltersSpec extends AnyWordSpec with Matchers {

  "A Filters" should {

    "use kalman filter for removing noise from measurements" in {
      val result = Filters.kalman(
        values = List(50.45, 50.967, 51.6, 52.106, 52.492, 52.819, 53.433, 54.007, 54.523, 54.99).reverse,
        gain = 0.6,
      )

      result mustBe List(
        54.984846832995395,
        54.520585170802306,
        54.00971456176093,
        53.45703742116872,
        52.81074030354454,
        52.483201268549855,
        52.10653708327552,
        51.62609993204602,
        51.016345124460344,
        50.45
      )
    }

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
