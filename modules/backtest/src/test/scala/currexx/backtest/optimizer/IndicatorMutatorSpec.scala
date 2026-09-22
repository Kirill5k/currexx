package currexx.backtest.optimizer

import cats.effect.IO
import cats.syntax.traverse.*
import currexx.backtest.optimizer.bounds.GeneBounds
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
          ind = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.Kalman(0.5, 1.0))
          res <- mutator.mutate(ind, 1.0d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.Kalman(0.45, 1.0))
        }
      }
    }

    "mutating trend-change-detection with StandardDeviation" should {
      "generate numbers in an expected range" in {
        given Random = Random(100)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          ind = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.StandardDeviation(30))
          res <- mutator.mutate(ind, 1.0d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.StandardDeviation(22))
        }
      }

      "not return values below minimum" in {
        given Random = Random(1)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          ind = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.StandardDeviation(5))
          res <- mutator.mutate(ind, 1.0d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.StandardDeviation(5))
        }
      }
    }

    "mutating BollingerBands indicator" should {
      "mutate all parameters correctly" in {
        given Random = Random(50)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          ind = Indicator.BollingerBands(ValueSource.Close, ValueTransformation.SMA(20), 20, 2.0)
          res <- mutator.mutate(ind, 1.0d)
        yield res

        result.asserting { ind =>
          // The pair starts at 20/20, a ratio of 1.0, and has to move as a pair. The band steps to 31 and the deviation window follows the
          // mutated ratio rather than staying near its own old value: the ratio mutates 1.0 -> 1.29, and 31 x 1.29 is 40. Reading the
          // ratio after the anchor had already moved, as the first cut of this did, is an independent walk wearing a ratio's clothes - it
          // would have measured 20/31 and returned a deviation near 20.
          ind mustBe Indicator.BollingerBands(ValueSource.Close, ValueTransformation.SMA(31), 40, 2.0)
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
          ind mustBe Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(10))
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
              ValueTransformation.Kalman(0.1, 1.0)
            )
          )
          res <- mutator.mutate(ind, 1.0d)
        yield res

        result.asserting { ind =>
          ind mustBe Indicator.TrendChangeDetection(
            ValueSource.Close,
            ValueTransformation.sequenced(
              ValueTransformation.HMA(10),
              ValueTransformation.Kalman(0.13, 1.0)
            )
          )
        }
      }
    }

    "mutating JMA power" should {
      "take an inward neighbouring step at either boundary" in {
        val result = List(1, 10).traverse { power =>
          given Random = controlledRandom(List(0.9, 0.9, 0.0, 0.0))
          IndicatorMutator.make[IO].flatMap(_.mutate(jma(power), 0.1))
        }

        result.asserting(_ mustBe List(jma(2), jma(9)))
      }

      "visit either neighbour of an interior power" in {
        val result = List(false, true).traverse { upwards =>
          given Random = controlledRandom(List(0.9, 0.9, 0.0, 0.0), upwards = upwards)
          IndicatorMutator.make[IO].flatMap(_.mutate(jma(5), 0.1))
        }

        result.asserting(_ mustBe List(jma(4), jma(6)))
      }

      "still require the per-gene mutation coin flip" in {
        given Random = controlledRandom(List(0.9, 0.9, 0.2))

        IndicatorMutator.make[IO].flatMap(_.mutate(jma(1), 0.1)).asserting(_ mustBe jma(1))
      }

      "leave the indicator unchanged with zero mutation probability" in {
        given Random = controlledRandom(List(0.0, 0.0, 0.0))

        IndicatorMutator.make[IO].flatMap(_.mutate(jma(1), 0.0)).asserting(_ mustBe jma(1))
      }

      "retain Gaussian steps larger than one and clamp them to the power bounds" in {
        val result = List(2.0, -100.0, 100.0).traverse { gaussian =>
          given Random = controlledRandom(List(0.9, 0.9, 0.0, 0.9), gaussian = gaussian)
          IndicatorMutator.make[IO].flatMap(_.mutate(jma(5), 0.1))
        }

        result.asserting(_ mustBe List(jma(8), jma(1), jma(10)))
      }

      "retain wider Gaussian jitter for initialisation" in {
        given Random = controlledRandom(List(0.9, 0.9, 0.0, 0.9), gaussian = 1.0)

        IndicatorMutator.scaled[IO](6.0).flatMap(_.mutate(jma(1), 0.1)).asserting(_ mustBe jma(4))
      }

      "leave power unchanged when the jitter scale is zero" in {
        given Random = controlledRandom(List(0.9, 0.9, 0.0))

        IndicatorMutator.scaled[IO](0.0).flatMap(_.mutate(jma(1), 0.1)).asserting(_ mustBe jma(1))
      }
    }

    "step a proportional gene by the same fraction of itself wherever it sits in its range" in {
      // The property log-space buys, and the reason `GeneBounds.Scale` exists. A linear tenth of [5, 100] is 9.5 bars: from a fast line at
      // 10 that is a rewrite, and from a slow one at 50 it is a nudge, so a linear walk would show roughly a fivefold difference in the
      // spread of the ratios below. Measured as a ratio rather than a difference because that is the unit the gene actually has.
      given Random = Random(200)

      def ratiosFrom(length: Int): IO[List[Double]] =
        for
          mutator <- IndicatorMutator.make[IO]
          mutated <- List.fill(400)(Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(length))).traverse {
            mutator.mutate(_, 1.0).map(lengthOf)
          }
        yield mutated.map(_.toDouble / length)

      val result = for
        low  <- ratiosFrom(10)
        high <- ratiosFrom(50)
      yield (spreadOf(low), spreadOf(high))

      result.asserting { case (low, high) =>
        low mustBe high +- (high * 0.25)
      }
    }
  }

  private def jma(power: Int): Indicator =
    Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.JMA(20, 0, power))

  private def controlledRandom(draws: List[Double], upwards: Boolean = false, gaussian: Double = 0.0): Random =
    new Random(0L) {
      private val remaining               = draws.iterator
      override def nextDouble(): Double   = remaining.next()
      override def nextBoolean(): Boolean = upwards
      override def nextGaussian(): Double = gaussian
    }

  /** The one length of a single-transformation indicator, so a test can read what the mutator did to it. */
  private def lengthOf(indicator: Indicator): Int = indicator match
    case Indicator.TrendChangeDetection(_, vt) => GeneBounds.lengthOf(vt).getOrElse(fail(s"no length gene in $vt"))
    case other                                 => fail(s"mutation changed the shape of the indicator: $other")

  private def spreadOf(values: List[Double]): Double =
    val mean = values.sum / values.size
    math.sqrt(values.map(v => (v - mean) * (v - mean)).sum / values.size)
}
