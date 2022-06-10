package currexx.algorithms.operators

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class CrossoverSpec extends AnyWordSpec with Matchers {

  "A threeWaySplitCrossover" should {

    "cross 2 parents into a child" in {
      given Random(1000)

      val p1 = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val p2 = p1.reverse

      val child = Crossover.threeWaySplit[Int].cross(p1, p2)

      child must contain theSameElementsAs p1
      child must contain theSameElementsAs p2
      child must not contain theSameElementsInOrderAs(p1)
      child must not contain theSameElementsInOrderAs(p2)
      child mustBe Array(1, 2, 5, 4, 3, 6, 7, 8, 9, 10)
    }
  }
}
