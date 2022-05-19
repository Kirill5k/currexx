package currexx.calculations

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FiltersSpec extends AnyWordSpec with Matchers {

  "A Filters" should {

    "use kalman filter for removing noise from measurements" in {
      val result = Filters.kalman(
        values = List(50.45, 50.967, 51.6, 52.106, 52.492, 52.819, 53.433, 54.007, 54.523, 54.99).reverse,
        measurementError = 0.1,
        noiseVariance = 0.0001,
        initialGuess = 10,
        initializationEstimateError = 100
      )

      result mustBe List(
        52.92531824447727,
        52.62631779251975,
        52.33076829099416,
        52.04460275027512,
        51.77872905021758,
        51.54805687662509,
        51.29449410930005,
        51.01141047992115,
        50.70976581732428,
        50.44995955004085
      )
    }

    "use kalman filter for removing noise from measurements of moving objects" in {
      val result = Filters.kalmanWithVelocity(
        values = List(30110D, 30265D, 30740D, 30750D, 31135D, 31015D, 31180D, 31610D, 31960D, 31865D).reverse,
        alpha = 0.2,
        beta = 0.1,
        initialGuess = 30000,
        initialVelocity = 40,
        time = 5
      )

      result mustBe List(31952.873160384, 31764.32870592, 31529.3570496, 31333.222848, 31176.402240000003, 31001.451200000003, 30769.456000000002, 30573.280000000002, 30351.4, 30182.0)
    }
  }
}
