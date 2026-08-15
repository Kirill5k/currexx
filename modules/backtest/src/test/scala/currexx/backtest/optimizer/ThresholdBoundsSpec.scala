package currexx.backtest.optimizer

import cats.effect.IO
import cats.syntax.traverse.*
import currexx.domain.signal.{Condition, Indicator, ValueSource, ValueTransformation}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class ThresholdBoundsSpec extends IOWordSpec {

  private def cmf(ub: Double, lb: Double) = Indicator.ThresholdCrossing(ValueSource.Close, ValueTransformation.CMF(20), ub, lb)
  private def rsx(ub: Double, lb: Double) = Indicator.ThresholdCrossing(ValueSource.Close, ValueTransformation.RSX(16), ub, lb)

  private def boundsOf(ind: Indicator): (Double, Double) = ind match
    case Indicator.ThresholdCrossing(_, _, ub, lb) => (ub, lb)
    case other                                     => fail(s"expected a ThresholdCrossing, got $other")

  /** Every value CMF can take, walked in order, as the detector would see it: head is the most recent bar. */
  private val cmfSweep: List[Double] = (-100 to 100).map(_ / 100.0).toList

  private def firesSomewhereOn(line: List[Double], ub: Double, lb: Double): Boolean =
    line.sliding(2).exists {
      case previous :: current :: Nil => Condition.thresholdCrossing(List(current, previous), lb, ub).isDefined
      case _                          => false
    }

  "ThresholdBounds" when {

    "mutating a CMF threshold" should {

      "keep both boundaries inside the range CMF can reach, however long the search runs" in {
        given Random = Random(42)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          evolved <- (1 to 500).toList.foldLeft(IO.pure(cmf(0.17, -0.17)): IO[Indicator]) { (acc, _) =>
            acc.flatMap(ind => mutator.mutate(ind, 1.0d))
          }
        yield boundsOf(evolved)

        result.asserting { (ub, lb) =>
          withClue(s"ub=$ub lb=$lb: ") {
            (ub >= -1.0 && ub <= 1.0) mustBe true
            (lb >= -1.0 && lb <= 1.0) mustBe true
            (ub >= 0.0) mustBe true
            (lb <= 0.0) mustBe true
            (lb <= ub) mustBe true
          }
        }
      }

      "repair a lower boundary on the wrong side of zero even when mutation does not fire" in {
        given Random = Random(42)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          mutated <- mutator.mutate(cmf(0.17, 0.40), 0.0d)
        yield boundsOf(mutated)

        result.asserting { (ub, lb) =>
          ub mustBe 0.17
          lb mustBe 0.0
        }
      }

      "leave a detector that a CMF line still crosses" in {
        given Random = Random(42)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          evolved <- (1 to 500).toList.foldLeft(IO.pure(cmf(0.17, -0.17)): IO[Indicator]) { (acc, _) =>
            acc.flatMap(ind => mutator.mutate(ind, 1.0d))
          }
        yield boundsOf(evolved)

        result.asserting { (ub, lb) =>
          withClue(s"ub=$ub lb=$lb never fires on a full CMF sweep: ") {
            firesSomewhereOn(cmfSweep, ub, lb) mustBe true
          }
        }
      }
    }

    "mutating a percentage-oscillator threshold" should {

      "search the same band the hardcoded constants used to" in {
        given Random = Random(7)

        val result = for
          mutator <- IndicatorMutator.make[IO]
          mutated <- (1 to 200).toList.traverse(_ => mutator.mutate(rsx(70.0, 30.0), 1.0d))
        yield mutated.map(boundsOf)

        result.asserting { bounds =>
          withClue(s"$bounds: ") {
            bounds.forall((ub, _) => ub >= 50.0 && ub <= 95.0) mustBe true
            bounds.forall((_, lb) => lb >= 5.0 && lb <= 50.0) mustBe true
            bounds.forall((ub, lb) => lb <= ub) mustBe true
          }
        }
      }
    }

    "crossing two CMF thresholds" should {

      "keep fractional boundaries rather than truncating them to zero" in {
        given Random = Random(11)

        val result = for
          crossover <- IndicatorCrossover.make[IO]
          crossed   <- (1 to 200).toList.traverse(_ => crossover.cross(cmf(0.17, -0.17), cmf(0.25, -0.30)))
        yield crossed.map(boundsOf)

        result.asserting { bounds =>
          withClue(s"$bounds: ") {
            bounds.forall((ub, _) => ub >= 0.17 && ub <= 0.25) mustBe true
            bounds.forall((_, lb) => lb >= -0.30 && lb <= -0.17) mustBe true
            bounds.forall((ub, lb) => ub >= 0.0 && lb <= 0.0) mustBe true
          }
        }
      }
    }

    "initialising a CMF threshold population" should {

      "seed every member inside the range CMF can reach" in {
        given Random = Random(13)

        val result = for
          initialiser <- IndicatorInitialiser.make[IO]
          population  <- initialiser.initialisePopulation(cmf(0.17, -0.17), 200, shuffle = true)
        yield population.map(boundsOf).toList

        result.asserting { bounds =>
          withClue(s"$bounds: ") {
            bounds.size mustBe 200
            bounds.forall((ub, lb) => ub >= -1.0 && ub <= 1.0 && lb >= -1.0 && lb <= 1.0) mustBe true
            bounds.forall((ub, lb) => ub >= 0.0 && lb <= 0.0) mustBe true
            bounds.forall((ub, lb) => lb <= ub) mustBe true
            bounds.forall((ub, lb) => firesSomewhereOn(cmfSweep, ub, lb)) mustBe true
          }
        }
      }
    }

    "initialising a percentage-oscillator threshold population" should {

      "keep the lower boundary in the lower half of the oscillator range" in {
        given Random = Random(17)

        val result = for
          initialiser <- IndicatorInitialiser.make[IO]
          population  <- initialiser.initialisePopulation(rsx(70.0, 30.0), 200, shuffle = true)
        yield population.map(boundsOf).toList

        result.asserting { bounds =>
          withClue(s"$bounds: ") {
            bounds.forall((ub, lb) => ub >= 50.0 && ub <= 95.0 && lb >= 5.0 && lb <= 50.0) mustBe true
          }
        }
      }
    }

    "of" should {

      "let a range-establishing final transformation replace its input range" in {
        val sequenced = ValueTransformation.sequenced(ValueTransformation.SMA(5), ValueTransformation.CMF(20))
        ThresholdBounds.of(sequenced) mustBe ThresholdBounds.of(ValueTransformation.CMF(20))
      }

      "preserve a CMF range through trailing smoothing" in {
        val sequenced = ValueTransformation.sequenced(ValueTransformation.CMF(20), ValueTransformation.SMA(5))
        ThresholdBounds.of(sequenced) mustBe ThresholdBounds.of(ValueTransformation.CMF(20))
      }

      "reproduce the hardcoded percentage constants for oscillators measured in percent" in {
        val band = ThresholdBounds.of(ValueTransformation.RSX(16))
        band.upperMin mustBe 50.0
        band.upperMax mustBe 95.0
        band.lowerMin mustBe 5.0
        band.lowerMax mustBe 50.0
        band.step mustBe 1.0
      }
    }
  }
}
