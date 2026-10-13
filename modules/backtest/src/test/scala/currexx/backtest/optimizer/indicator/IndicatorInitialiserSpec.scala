package currexx.backtest.optimizer.indicator

import cats.effect.IO
import cats.syntax.traverse.*
import currexx.backtest.optimizer.indicator.bounds.{GeneBounds, IndicatorBounds}
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
    * silently moves it somewhere else - and every related pair has to hold its ratio, or the operators inherit a candidate they would have
    * been forbidden to produce. `IndicatorBounds.isValid` is the single definition of both.
    */
  def genesWithinBounds(indicator: Indicator): Boolean = IndicatorBounds.isValid(indicator)

  /** Whether a member is drawn from the seed's own neighbourhood, read off the trend line because it is the gene the seed pins furthest
    * from the middle of its range: JMA(90) against a searchable [5, 100], where an independent draw is log-uniform and lands low.
    */
  def nearSeed(indicator: Indicator): Boolean = indicator match
    case Indicator.Composite(is, _)            => is.toList.exists(nearSeed)
    case Indicator.TrendChangeDetection(_, vt) => GeneBounds.lengthOf(vt).exists(_ >= 45)
    case _                                     => false

  "An IndicatorInitialiser" when {

    "using the shared population builder" should {
      "preserve the previous population ordering and random sequence in both mixes and small budgets" in {
        val base       = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(20))
        val sibling    = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(30))
        val extras     = List(sibling, sibling, differentShape, base)
        val compatible = Vector(base, sibling)

        // Keep the previous indicator allocation as a regression oracle for the shared builder.
        def previous(size: Int, shuffle: Boolean): IO[(Vector[Indicator], Double)] = {
          given Random = Random(911)
          val clones   = math.min(size, math.max(compatible.size, (size * (if (shuffle) 0.15 else 0.30)).toInt))
          val jittered = math.min(size - clones, (size * (if (shuffle) 0.55 else 0.60)).toInt)
          val radii    = if (shuffle) List(1.0, 3.0, 6.0) else List(1.0)
          for
            mutators   <- radii.traverse(IndicatorMutator.scaled[IO])
            neighbours <- Vector
              .range(0, jittered)
              .traverse(index => mutators(index % mutators.size).mutate(compatible(index % compatible.size), 1.0))
            immigrants <- Vector.range(0, size - clones - jittered).traverse(_ => IndicatorInitialiser.randomise[IO](base))
            next       <- IO(summon[Random].nextDouble())
          yield (Vector.tabulate(clones)(index => compatible(index % compatible.size)) ++ neighbours ++ immigrants, next)
        }
        def shared(size: Int, shuffle: Boolean): IO[(Vector[Indicator], Double)] = {
          given Random = Random(911)
          for
            initialiser <- IndicatorInitialiser.seeded[IO](extras)
            population  <- initialiser.initialisePopulation(base, size, shuffle)
            next        <- IO(summon[Random].nextDouble())
          yield (population, next)
        }
        List((31, true), (31, false), (2, true))
          .traverse { (size, shuffle) =>
            for expected <- previous(size, shuffle); actual <- shared(size, shuffle) yield (expected, actual)
          }
          .asserting { comparisons =>
            comparisons.foreach { case (expected, actual) => actual mustBe expected }
            succeed
          }
      }
    }

    "shuffle is false" should {
      "keep thirty copies of the seed and draw the remaining seventy" in {
        // Most refining candidates are now neighbours of the seed, giving crossover more distinct parents while preserving the seed.
        given Random = Random(42)
        val result   = for
          init <- IndicatorInitialiser.make[IO]
          pop  <- init.initialisePopulation(seed, 100, false)
        yield pop

        result.asserting { pop =>
          pop must have size 100
          pop.take(30) mustBe Vector.fill(30)(seed)
          pop.distinct.size must be > 50
          pop.filterNot(genesWithinBounds) mustBe Vector.empty
        }
      }

      "stay nearer the seed than a shuffled draw does" in {
        // Refining puts 90% of the population in seed copies or one-step neighbours; exploring adds wider jitter and more immigrants.
        given Random = Random(42)
        val result   = for
          init      <- IndicatorInitialiser.make[IO]
          refining  <- init.initialisePopulation(seed, 200, false)
          exploring <- init.initialisePopulation(seed, 200, true)
        yield (refining.count(nearSeed), exploring.count(nearSeed))

        result.asserting { case (refining, exploring) =>
          refining must be > exploring
        }
      }

      "mix in extra seeds of the same shape" in {
        given Random = Random(99)
        val sibling  = Indicator.compositeAnyOf(
          Indicator.TrendChangeDetection(ValueSource.HLC3, VT.JMA(length = 50, phase = -6, power = 1)),
          Indicator.BollingerBands(ValueSource.Close, VT.SMA(35), stdDevLength = 41, stdDevMultiplier = 2.6),
          Indicator.VolatilityRegimeDetection(atrLength = 28, smoothingType = VT.SMA(63)),
          Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(11), upperBoundary = 66.0, lowerBoundary = 30.0),
          Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(8))
        )
        val result = for
          init <- IndicatorInitialiser.seeded[IO](List(sibling))
          pop  <- init.initialisePopulation(seed, 100, false)
        yield pop

        result.asserting { pop =>
          pop must have size 100
          pop.take(30).count(_ == seed) mustBe 15
          pop.take(30).count(_ == sibling) mustBe 15
          pop must contain(seed)
          pop must contain(sibling)
        }
      }
    }

    "shuffle is true" should {
      "keep the seed in the population and draw the rest around it" in {
        given Random = Random(42)
        val result   = for
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
        val result   = for
          init <- IndicatorInitialiser.make[IO]
          pop  <- init.initialisePopulation(seed, 300, true)
        yield pop

        result.asserting(pop => pop.filterNot(genesWithinBounds) mustBe Vector.empty)
      }

      "produce only members that can be crossed with each other" in {
        given Random = Random(13)
        val result   = for
          init      <- IndicatorInitialiser.make[IO]
          crossover <- IndicatorCrossover.make[IO]
          pop       <- init.initialisePopulation(seed, 60, true)
          crossed   <- pop.toList.traverse(crossover.cross(_, seed, 1.0))
        yield crossed

        result.asserting(_ must have size 60)
      }

      "mix in extra seeds of the same shape" in {
        given Random = Random(99)
        val sibling  = Indicator.compositeAnyOf(
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

      "reject a population that cannot retain every distinct seed" in {
        given Random = Random(5)
        val siblings = List
          .range(10, 40)
          .map(l =>
            Indicator.compositeAnyOf(
              Indicator.TrendChangeDetection(ValueSource.HLC3, VT.JMA(length = l, phase = -6, power = 1)),
              Indicator.BollingerBands(ValueSource.Close, VT.SMA(35), stdDevLength = 41, stdDevMultiplier = 2.6),
              Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = VT.SMA(50)),
              Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(11), upperBoundary = 66.0, lowerBoundary = 30.0),
              Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(8))
            )
          )
        val result = for
          init   <- IndicatorInitialiser.seeded[IO](siblings)
          result <- init.initialisePopulation(seed, 20, true).attempt
        yield result

        result.asserting(_.left.map(_.getMessage) mustBe Left("Population size must be positive and hold all distinct seeds"))
      }

      "drop extra seeds that could not be crossed with the target" in {
        given Random = Random(99)
        val result   = for
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
