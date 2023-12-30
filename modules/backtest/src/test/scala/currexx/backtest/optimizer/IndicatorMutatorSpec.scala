package currexx.backtest.optimizer

import cats.effect.IO
import currexx.domain.market.{Indicator, ValueSource, ValueTransformation}

import scala.util.Random

class IndicatorMutatorSpec extends CatsSpec {

  "An IndicatorMutator" when {

    "mutating trend-change-detection with Kalman" should {
      "generate numbers in an expected range" in {
        given Random = Random(100)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          ind = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.Kalman(0.5))
          res <- mutator.mutate(ind, 0.2d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.Kalman(0.1))
        }
      }
    }

    "mutating trend-change-detection with HMA" should {
      "generate numbers in an expected range" in {
        given Random = Random(100)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          ind = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(14))
          res <- mutator.mutate(ind, 0.2d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(30))
        }
      }

      "not return 0" in {
        given Random = Random(1)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          ind = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(0))
          res <- mutator.mutate(ind, 0.2d)
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
          res <- mutator.mutate(ind, 0.2d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(
            ValueSource.Close,
            ValueTransformation.sequenced(
              ValueTransformation.HMA(30),
              ValueTransformation.Kalman(0.1)
            )
          )
        }
      }
    }
  }
}
