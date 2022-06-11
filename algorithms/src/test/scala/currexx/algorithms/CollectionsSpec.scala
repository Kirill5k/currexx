package currexx.algorithms

import org.scalatest.wordspec.AnyWordSpec

import org.scalatest.Inspectors
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CollectionsSpec extends AnyWordSpec with Matchers with Inspectors {

  import collections.*

  "Vector.pairs extension" should {
    "distribute list elements in pairs" in {
      val result = Vector(1, 2, 3, 4, 5, 6, 7, 8).pairs
      result mustBe Vector((1, 2), (3, 4), (5, 6), (7, 8))
    }
  }
}
