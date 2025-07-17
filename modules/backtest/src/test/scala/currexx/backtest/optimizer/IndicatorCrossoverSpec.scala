package currexx.backtest.optimizer

import cats.effect.IO
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class IndicatorCrossoverSpec extends IOWordSpec {

  "An IndicatorCrossover" should {
    "cross 2 indicators with HMA transformations together" in {
      given Random = Random(1)

      val result = for
        cross <- IndicatorCrossover.make[IO]
        ind1 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(40))
        ind2 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(10))
        result <- cross.cross(ind1, ind2)
      yield result

      result.asserting { ind =>
        ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(13))
      }
    }

    "cross 2 indicators with sequenced transformations" in {
      given Random = Random(10)

      val result = for
        cross <- IndicatorCrossover.make[IO]
        ind1 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.sequenced(
          ValueTransformation.HMA(40),
          ValueTransformation.Kalman(0.7),
        ))
        ind2 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.sequenced(
          ValueTransformation.HMA(37),
          ValueTransformation.Kalman(0.6),
        ))
        result <- cross.cross(ind1, ind2)
      yield result

      result.asserting { ind =>
        ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.sequenced(
          ValueTransformation.HMA(38),
          ValueTransformation.Kalman(0.65),
        ))
      }
    }

    "return error when indicators have different types" in {
      given Random = Random(10)

      val result = for
        cross <- IndicatorCrossover.make[IO]
        ind1 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.sequenced(
          ValueTransformation.HMA(40),
          ValueTransformation.Kalman(0.7),
        ))
        ind2 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(37))
        result <- cross.cross(ind1, ind2)
      yield result

      result.attempt.asserting { res =>
        res.left.map(_.getMessage) mustBe Left("failed to cross TrendChangeDetection(Close,Sequenced(List(HMA(40), Kalman(0.7)))) and TrendChangeDetection(Close,HMA(37)) together: both parents must be of the same type")
      }
    }
  }
}
