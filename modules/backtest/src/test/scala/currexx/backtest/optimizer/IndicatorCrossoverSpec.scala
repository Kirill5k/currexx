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

    "cross 2 indicators with StandardDeviation transformations" in {
      given Random = Random(1)

      val result = for
        cross <- IndicatorCrossover.make[IO]
        ind1 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.StandardDeviation(30))
        ind2 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.StandardDeviation(20))
        result <- cross.cross(ind1, ind2)
      yield result

      result.asserting { ind =>
        ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.StandardDeviation(21))
      }
    }

    "cross 2 BollingerBands indicators" in {
      given Random = Random(5)

      val result = for
        cross <- IndicatorCrossover.make[IO]
        ind1 = Indicator.BollingerBands(ValueSource.Close, ValueTransformation.SMA(20), 20, 2.0)
        ind2 = Indicator.BollingerBands(ValueSource.Close, ValueTransformation.SMA(30), 25, 2.5)
        result <- cross.cross(ind1, ind2)
      yield result

      result.asserting { ind =>
        // One weight for the whole indicator: alpha of 0.2 takes the band from 20/30 to 28, and the same 0.2 puts the deviation window at
        // 24 and the multiplier at 2.4. The three genes used to be blended at three independent weights, which is how a crossover of two
        // valid parents reached a pair neither of them held.
        ind mustBe Indicator.BollingerBands(ValueSource.Close, ValueTransformation.SMA(28), 24, 2.4)
      }
    }

    "cross 2 indicators with sequenced transformations" in {
      given Random = Random(10)

      val result = for
        cross <- IndicatorCrossover.make[IO]
        ind1 = Indicator.TrendChangeDetection(
          ValueSource.Close,
          ValueTransformation.sequenced(
            ValueTransformation.HMA(40),
            ValueTransformation.Kalman(0.7, 1.0)
          )
        )
        ind2 = Indicator.TrendChangeDetection(
          ValueSource.Close,
          ValueTransformation.sequenced(
            ValueTransformation.HMA(37),
            ValueTransformation.Kalman(0.6, 1.0)
          )
        )
        result <- cross.cross(ind1, ind2)
      yield result

      result.asserting { ind =>
        // Both parents' Kalman gains sit above `GeneBounds.kalmanGain`, whose ceiling is 0.5, so the blend of them does too and the repair
        // pass pulls it in. The old crossover returned 0.65 - a value the mutator would have clamped the first time it touched the gene.
        ind mustBe Indicator.TrendChangeDetection(
          ValueSource.Close,
          ValueTransformation.sequenced(
            ValueTransformation.HMA(38),
            ValueTransformation.Kalman(0.5, 1.0)
          )
        )
      }
    }

    "copy a parent's genes untouched at the extreme weights" in {
      // Interpolating a parent with itself is not the identity once the result is snapped to a grid: 0.17 on a CMF band's 0.02 step lands
      // on 0.18, and the clamp to the parental interval does not catch it when the other parent is higher. Off-grid values are legal, so a
      // copy has to be a copy.
      IndicatorCrossover.Blend(1.0).double(0.17, 0.20, 0.02) mustBe 0.17
      IndicatorCrossover.Blend(0.0).double(0.20, 0.17, 0.02) mustBe 0.17
      IndicatorCrossover.Blend(1.0).int(19, 42) mustBe 19
      IndicatorCrossover.Blend(0.0).int(42, 19) mustBe 19
    }

    "pair two parents whose lines are the same types the other way round" in {
      given Random = Random(3)

      // sameShape used to ask positionally and reject this, while the crossover could handle it by swapping - so the initialiser dropped
      // seeds the crossover would have accepted. Both now ask linePairing.
      val ind1 = Indicator.LinesCrossing(ValueSource.HLC3, ValueTransformation.SMA(10), ValueTransformation.EMA(40))
      val ind2 = Indicator.LinesCrossing(ValueSource.HLC3, ValueTransformation.EMA(44), ValueTransformation.SMA(12))

      IndicatorCrossover.sameShape(ind1, ind2) mustBe true

      val result = for
        cross  <- IndicatorCrossover.make[IO]
        result <- cross.cross(ind1, ind2)
      yield result

      result.asserting { ind =>
        // The fast SMA of one parent crossed with the fast SMA of the other, so the child keeps parent one's SMA-then-EMA ordering.
        ind mustBe Indicator.LinesCrossing(ValueSource.HLC3, ValueTransformation.SMA(11), ValueTransformation.EMA(42))
      }
    }

    "return error when indicators have different types" in {
      given Random = Random(10)

      val result = for
        cross <- IndicatorCrossover.make[IO]
        ind1 = Indicator.TrendChangeDetection(
          ValueSource.Close,
          ValueTransformation.sequenced(
            ValueTransformation.HMA(40),
            ValueTransformation.Kalman(0.7, 1.0)
          )
        )
        ind2 = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(37))
        result <- cross.cross(ind1, ind2)
      yield result

      result.attempt.asserting { res =>
        res.left.map(_.getMessage) mustBe Left(
          "failed to cross TrendChangeDetection(Close,Sequenced(List(HMA(40), Kalman(0.7,1.0)))) and TrendChangeDetection(Close,HMA(37)) together: both parent indicators must be of the same shape: TrendChangeDetection(Close,Sequenced(List(HMA(40), Kalman(0.7,1.0)))) vs TrendChangeDetection(Close,HMA(37))"
        )
      }
    }
  }
}
