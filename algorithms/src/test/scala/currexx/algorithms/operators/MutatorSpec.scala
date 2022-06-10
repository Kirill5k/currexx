package currexx.algorithms.operators

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class MutatorSpec extends AnyWordSpec with Matchers {

  "A neighbourSwapMutator" should {
    "swap 2 neighbour elements in seq" in {
      val individual = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      given r: Random   = Random(40)
      val mutatedTracks = Mutator.neighbourSwap[Int].mutate(individual, 0.25)

      mutatedTracks must not contain theSameElementsInOrderAs(individual)
      mutatedTracks must contain theSameElementsAs individual
      mutatedTracks must contain theSameElementsInOrderAs Array(1, 3, 2, 4, 5, 7, 6, 9, 8, 10)
    }
  }
}
