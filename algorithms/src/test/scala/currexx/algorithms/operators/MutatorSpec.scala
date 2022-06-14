package currexx.algorithms.operators

import cats.effect.IO
import currexx.algorithms.CatsSpec

import scala.util.Random

class MutatorSpec extends CatsSpec {

  "Mutator.neighbourSwap" should {
    "swap 2 neighbour elements in seq" in {
      val individual = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      given r: Random   = Random(40)
      val result = Mutator.neighbourSwap[IO, Int].flatMap(_.mutate(individual, 0.25))

      result.asserting { mutated =>
        mutated must not contain theSameElementsInOrderAs(individual)
        mutated must contain theSameElementsAs individual
        mutated must contain theSameElementsInOrderAs Array(1, 3, 2, 4, 5, 7, 6, 9, 8, 10)
      }
    }
  }
}
