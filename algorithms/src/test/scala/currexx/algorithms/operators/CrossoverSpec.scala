package currexx.algorithms.operators

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.util.Random

class CrossoverSpec extends AsyncWordSpec with Matchers {

  "A threeWaySplitCrossover" should {

    "cross 2 parents into a child" in {
      given Random(1000)

      val p1 = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val p2 = p1.reverse

      val result = Crossover.threeWaySplit[IO, Int].flatMap(_.cross(p1, p2))

      result.unsafeToFuture().map { child =>
        child must contain theSameElementsAs p1
        child must contain theSameElementsAs p2
        child must not contain theSameElementsInOrderAs(p1)
        child must not contain theSameElementsInOrderAs(p2)
        child mustBe Array(1, 2, 5, 4, 3, 6, 7, 8, 9, 10)
      }
    }
  }
}
