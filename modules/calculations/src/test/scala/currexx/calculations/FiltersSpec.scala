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
  }
}
