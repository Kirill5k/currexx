package currexx.algorithms.operators

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.algorithms.CatsSpec

import scala.util.Random

class CrossoverSpec extends CatsSpec {

  "Crossover.threeWaySplit" should {

    "cross 2 parents into a child" in {
      given Random(1000)

      val p1 = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val p2 = p1.reverse

      val result = Crossover.threeWaySplit[IO, Int].flatMap(_.cross(p1, p2))

      result.asserting { child =>
        child mustBe Array(1, 2, 8, 7, 6, 6, 7, 8, 9, 10)
      }
    }
  }
}
