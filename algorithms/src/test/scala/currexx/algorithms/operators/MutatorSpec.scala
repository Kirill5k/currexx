package currexx.algorithms.operators

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.util.Random

class MutatorSpec extends AsyncWordSpec with Matchers {

  "A neighbourSwapMutator" should {
    "swap 2 neighbour elements in seq" in {
      val individual = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      given r: Random   = Random(40)
      val result = Mutator.neighbourSwap[IO, Int].mutate(individual, 0.25)

      result.unsafeToFuture().map { mutated =>
        mutated must not contain theSameElementsInOrderAs(individual)
        mutated must contain theSameElementsAs individual
        mutated must contain theSameElementsInOrderAs Array(1, 3, 2, 4, 5, 7, 6, 9, 8, 10)
      }
    }
  }
}
