package currexx.algorithms.operators

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class MutatorSpec extends IOWordSpec {

  "Mutator.neighbourSwap" should {
    "swap 2 neighbour elements in seq" in {
      val individual = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      given Random = Random(40)
      val result   = Mutator.neighbourSwap[IO, Int].flatMap(_.mutate(individual, 0.25))

      result.asserting { mutated =>
        mutated must not contain theSameElementsInOrderAs(individual)
        mutated must contain theSameElementsAs individual
        (mutated must contain).theSameElementsInOrderAs(Array(1, 3, 2, 4, 5, 7, 6, 9, 8, 10))
      }
    }
  }

  "Mutator.bitFlip" should {
    "flip arbitrary bit in a genetic sequence from its original state" in {
      val individual = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

      given r: Random = Random(40)
      val result      = Mutator.bitFlip[IO].flatMap(_.mutate(individual, 0.25))

      result.asserting { mutated =>
        mutated mustBe Array(0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1)
      }
    }
  }
}
