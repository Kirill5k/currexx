package currexx.backtest.optimizer

import cats.effect.IO
import cats.syntax.traverse.*
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class IndicatorInitialiserSpec extends IOWordSpec {

  val seed: Indicator = Indicator.compositeAnyOf(
    Indicator.TrendChangeDetection(ValueSource.HLC3, VT.JMA(length = 90, phase = -6, power = 1)),
    Indicator.BollingerBands(ValueSource.Close, VT.SMA(35), stdDevLength = 41, stdDevMultiplier = 2.6),
    Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = VT.SMA(50)),
    Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(11), upperBoundary = 66.0, lowerBoundary = 30.0),
    Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(8))
  )

  val differentShape: Indicator = Indicator.compositeAnyOf(
    Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(19, 14, 2), VT.JMA(32, -43, 1)),
    Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(29), 74.0, 29.0),
    Indicator.VolatilityRegimeDetection(37, VT.SMA(35))
  )

  /** Every gene the initialiser draws has to sit inside the range the mutator is allowed to hold it in, or the first mutation to touch it
    * silently moves it somewhere else.
    */
  def genesWithinBounds(indicator: Indicator): Boolean = {
    def vtOk(vt: VT): Boolean = vt match
      case VT.Sequenced(sequence)     => sequence.forall(vtOk)
      case VT.JMA(length, phase, power) =>
        GeneBounds.jmaLength.clamp(length) == length && GeneBounds.jmaPhase.clamp(phase) == phase &&
        GeneBounds.jmaPower.clamp(power) == power
      case VT.Kalman(g, n)         => GeneBounds.kalmanGain.snap(g) == g && GeneBounds.kalmanNoise.snap(n) == n
      case VT.KalmanVelocity(g, n) => GeneBounds.kalmanGain.snap(g) == g && GeneBounds.kalmanNoise.snap(n) == n
      case VT.NMA(l, sl, lambda, _) =>
        GeneBounds.nmaLength.clamp(l) == l && GeneBounds.nmaSignalLength.clamp(sl) == sl && GeneBounds.nmaLambda.snap(lambda) == lambda
      case other => GeneBounds.lengthOf(other).forall(l => GeneBounds.lengthRange(other).clamp(l) == l)

    def indOk(ind: Indicator): Boolean = ind match
      case Indicator.Composite(is, _)                       => is.forall(indOk)
      case Indicator.TrendChangeDetection(_, vt)            => vtOk(vt)
      case Indicator.ThresholdCrossing(_, vt, _, _)         => vtOk(vt)
      case Indicator.LinesCrossing(_, vt1, vt2)             => vtOk(vt1) && vtOk(vt2)
      case Indicator.ValueTracking(_, _, vt)                => vtOk(vt)
      case Indicator.PriceLineCrossing(_, _, vt)            => vtOk(vt)
      case Indicator.VolatilityRegimeDetection(atrL, vt)    => GeneBounds.atrLength.clamp(atrL) == atrL && vtOk(vt)
      case Indicator.KeltnerChannel(_, md, atrL, atrM)      =>
        vtOk(md) && GeneBounds.atrLength.clamp(atrL) == atrL && GeneBounds.keltnerMultiplier.snap(atrM) == atrM
      case Indicator.BollingerBands(_, md, sdl, sdm) =>
        vtOk(md) && GeneBounds.stdDevLength.clamp(sdl) == sdl && GeneBounds.bollingerMultiplier.snap(sdm) == sdm

    indOk(indicator)
  }

  "An IndicatorInitialiser" when {

    "shuffle is false" should {
      "return the seed unchanged, as many times as asked" in {
        given Random = Random(42)
        val result = for
          init <- IndicatorInitialiser.make[IO]
          pop  <- init.initialisePopulation(seed, 20, false)
        yield pop

        result.asserting { pop =>
          pop must have size 20
          pop.toSet mustBe Set(seed)
        }
      }
    }

    "shuffle is true" should {
      "keep the seed in the population and draw the rest around it" in {
        given Random = Random(42)
        val result = for
          init <- IndicatorInitialiser.make[IO]
          pop  <- init.initialisePopulation(seed, 100, true)
        yield pop

        result.asserting { pop =>
          pop must have size 100
          pop.count(_ == seed) mustBe 15
          pop.distinct.size must be > 50
        }
      }

      "draw every gene inside the range the mutator can hold" in {
        given Random = Random(7)
        val result = for
          init <- IndicatorInitialiser.make[IO]
          pop  <- init.initialisePopulation(seed, 300, true)
        yield pop

        result.asserting(pop => pop.filterNot(genesWithinBounds) mustBe Vector.empty)
      }

      "produce only members that can be crossed with each other" in {
        given Random = Random(13)
        val result = for
          init     <- IndicatorInitialiser.make[IO]
          crossover <- IndicatorCrossover.make[IO]
          pop      <- init.initialisePopulation(seed, 60, true)
          crossed  <- pop.toList.traverse(crossover.cross(_, seed, 1.0))
        yield crossed

        result.asserting(_ must have size 60)
      }

      "mix in extra seeds of the same shape" in {
        given Random = Random(99)
        val sibling = Indicator.compositeAnyOf(
          Indicator.TrendChangeDetection(ValueSource.HLC3, VT.JMA(length = 50, phase = -6, power = 1)),
          Indicator.BollingerBands(ValueSource.Close, VT.SMA(35), stdDevLength = 41, stdDevMultiplier = 2.6),
          Indicator.VolatilityRegimeDetection(atrLength = 28, smoothingType = VT.SMA(63)),
          Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(11), upperBoundary = 66.0, lowerBoundary = 30.0),
          Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(8))
        )
        val result = for
          init <- IndicatorInitialiser.seeded[IO](List(sibling))
          pop  <- init.initialisePopulation(seed, 100, true)
        yield pop

        result.asserting { pop =>
          pop must contain(seed)
          pop must contain(sibling)
        }
      }

      "return exactly the requested size when there are more seeds than the clone share allows" in {
        given Random = Random(5)
        val siblings = List.range(10, 40).map(l =>
          Indicator.compositeAnyOf(
            Indicator.TrendChangeDetection(ValueSource.HLC3, VT.JMA(length = l, phase = -6, power = 1)),
            Indicator.BollingerBands(ValueSource.Close, VT.SMA(35), stdDevLength = 41, stdDevMultiplier = 2.6),
            Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = VT.SMA(50)),
            Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(11), upperBoundary = 66.0, lowerBoundary = 30.0),
            Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(8))
          )
        )
        val result = for
          init <- IndicatorInitialiser.seeded[IO](siblings)
          pop  <- init.initialisePopulation(seed, 20, true)
        yield pop

        result.asserting(_ must have size 20)
      }

      "drop extra seeds that could not be crossed with the target" in {
        given Random = Random(99)
        val result = for
          init <- IndicatorInitialiser.seeded[IO](List(differentShape))
          pop  <- init.initialisePopulation(seed, 100, true)
        yield pop

        result.asserting { pop =>
          pop must not contain differentShape
          pop.count(_ == seed) mustBe 15
        }
      }
    }
  }
}
