package currexx.backtest.optimizer

import currexx.domain.market.ValueTransformation
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

class OptimisableSpec extends AnyWordSpec with Matchers {

  "Optimisable[ValueTransformation]" when {
    val optimisable = Optimisable.given_Optimisable_ValueTransformation

    "toGenome" should {
      "convert ValueTransformation.Kalman to genome" in {
        val transformation = ValueTransformation.Kalman(0.1)
        optimisable.toGenome(transformation) mustBe Array(Array(1), Array(0, 0, 0, 1, 0, 1, 0))
      }

      "convert ValueTransformation.HMA to genome" in {
        val transformation = ValueTransformation.HMA(14)
        optimisable.toGenome(transformation) mustBe Array(Array(5), Array(0, 1, 1, 1, 0))
      }

      "convert ValueTransformation.Sequence to genome" in {
        val transformation = ValueTransformation.sequenced(
          ValueTransformation.Kalman(0.1),
          ValueTransformation.HMA(14)
        )
        optimisable.toGenome(transformation) mustBe Array(Array(1), Array(0, 0, 0, 1, 0, 1, 0), Array(5), Array(0, 1, 1, 1, 0))
      }
    }

    "fromGenome" should {
      "convert genome to ValueTransformation.Kalman" in {
        val genome = Array(Array(1), Array(0, 0, 0, 1, 0, 1, 0))

        optimisable.fromGenome(genome) mustBe ValueTransformation.sequenced(ValueTransformation.Kalman(0.1))
      }

      "convert genome to ValueTransformation.HMA" in {
        val genome = Array(Array(5), Array(0, 1, 1, 1, 0))

        optimisable.fromGenome(genome) mustBe ValueTransformation.sequenced(ValueTransformation.HMA(14))
      }

      "convert complex genome into multiple transformations" in {
        val genome = Array(Array(1), Array(0, 0, 0, 1, 0, 1, 0), Array(5), Array(0, 1, 1, 1, 0))

        optimisable.fromGenome(genome) mustBe ValueTransformation.sequenced(
          ValueTransformation.Kalman(0.1),
          ValueTransformation.HMA(14)
        )
      }
    }
  }
}
