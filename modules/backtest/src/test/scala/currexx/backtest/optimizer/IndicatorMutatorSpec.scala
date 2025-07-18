package currexx.backtest.optimizer

import cats.effect.IO
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class IndicatorMutatorSpec extends IOWordSpec {

  "An IndicatorMutator" when {

    "mutating trend-change-detection with Kalman" should {
      "generate numbers in an expected range" in {
        given Random = Random(100)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          ind = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.Kalman(0.5))
          res <- mutator.mutate(ind, 1.0d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.Kalman(0.45))
        }
      }
    }

    "mutating trend-change-detection with HMA" should {
      "generate numbers in an expected range" in {
        given Random = Random(100)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          ind = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(14))
          res <- mutator.mutate(ind, 1.0d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(5))
        }
      }

      "not return 0" in {
        given Random = Random(1)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          ind = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(0))
          res <- mutator.mutate(ind, 1.0d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(5))
        }
      }
    }

    "mutating trend-change-detection with sequenced transformations" should {
      "generate numbers in an expected range" in {
        given Random = Random(100)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          ind = Indicator.TrendChangeDetection(
            ValueSource.Close,
            ValueTransformation.sequenced(
              ValueTransformation.HMA(14),
              ValueTransformation.Kalman(0.1)
            )
          )
          res <- mutator.mutate(ind, 1.0d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(
            ValueSource.Close,
            ValueTransformation.sequenced(
              ValueTransformation.HMA(5),
              ValueTransformation.Kalman(0.13)
            )
          )
        }
      }
    }
  }
}
