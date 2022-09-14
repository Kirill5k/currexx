package currexx.backtest.optimizer

import cats.effect.IO
import currexx.domain.market.{Indicator, ValueSource, ValueTransformation}

import scala.util.Random

class IndicatorCrossoverSpec extends CatsSpec {

  "An IndicatorCrossover" should {
    "cross 2 indicators with HMA transformations together" in {
      given Random = Random(1)

      val result = for
        cross <- IndicatorCrossover.make[IO]
        ind1 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SingleOutput.HMA(40))
        ind2 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SingleOutput.HMA(10))
        result <- cross.cross(ind1, ind2)
      yield result

      result.asserting { ind =>
        ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SingleOutput.HMA(8))
      }
    }

    "cross 2 indicators with sequenced transformations" in {
      given Random = Random(10)

      val result = for
        cross <- IndicatorCrossover.make[IO]
        ind1 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SingleOutput.sequenced(
          ValueTransformation.SingleOutput.HMA(40),
          ValueTransformation.SingleOutput.Kalman(0.7),
        ))
        ind2 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SingleOutput.sequenced(
          ValueTransformation.SingleOutput.HMA(37),
          ValueTransformation.SingleOutput.Kalman(0.6),
        ))
        result <- cross.cross(ind1, ind2)
      yield result

      result.asserting { ind =>
        ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SingleOutput.sequenced(
          ValueTransformation.SingleOutput.HMA(32),
          ValueTransformation.SingleOutput.Kalman(0.5),
        ))
      }
    }

    "return error when indicators have different types" in {
      given Random = Random(10)

      val result = for
        cross <- IndicatorCrossover.make[IO]
        ind1 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SingleOutput.sequenced(
          ValueTransformation.SingleOutput.HMA(40),
          ValueTransformation.SingleOutput.Kalman(0.7),
        ))
        ind2 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SingleOutput.HMA(37))
        result <- cross.cross(ind1, ind2)
      yield result

      result.attempt.asserting { res =>
        res.left.map(_.getMessage) mustBe Left("both parents must be of the same type")
      }
    }
  }
}
