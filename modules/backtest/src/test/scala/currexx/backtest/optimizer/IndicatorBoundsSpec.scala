package currexx.backtest.optimizer

import cats.effect.IO
import cats.syntax.traverse.*
import currexx.backtest.TestStrategy
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class IndicatorBoundsSpec extends IOWordSpec {

  /** Every strategy the catalogue currently holds. The bands are meant to describe these rather than constrain them, so any one of them
    * that `repair` moves is a band set from theory instead of from measurement.
    */
  val catalogue: List[(String, Indicator)] = List(
    "s1_v2_optimized" -> TestStrategy.s1_v2_optimized,
    "s2_optimized"    -> TestStrategy.s2_optimized,
    "s2_optimized_v2" -> TestStrategy.s2_optimized_v2,
    "s2_optimized_v3" -> TestStrategy.s2_optimized_v3,
    "s4_optimized_v1" -> TestStrategy.s4_optimized_v1,
    "s4_optimized_v2" -> TestStrategy.s4_optimized_v2,
    "s5_optimized_v2" -> TestStrategy.s5_optimized_v2,
    "s6"              -> TestStrategy.s6,
    "s12"             -> TestStrategy.s12,
    "s12_optimized"   -> TestStrategy.s12_optimized
  ).map((name, strategy) => name -> strategy.indicator)

  "IndicatorBounds.repair" when {

    "given a strategy from the catalogue" should {
      "leave every one of them exactly as it is" in {
        val moved = catalogue.collect { case (name, ind) if IndicatorBounds.repair(ind) != ind => name }
        moved mustBe Nil
      }

      "leave s2_optimized's squeeze inverted, because it is the second-best val in the file at a ratio of 0.946" in {
        val squeeze = Indicator.VolatilityRegimeDetection(atrLength = 37, smoothingType = VT.SMA(35))
        IndicatorBounds.repair(squeeze) mustBe squeeze
      }
    }

    "given the failures that motivated it" should {
      "push a squeeze smoothed over a shorter window than its own ATR back into the band" in {
        // The champion of ga-optimisation-2026-09-02-0722, reached by mutating the two lengths independently.
        val inverted = Indicator.VolatilityRegimeDetection(atrLength = 22, smoothingType = VT.SMA(6))
        // Repaired to the floor of the validity band rather than to a textbook squeeze: the floor is 0.70 because s2_optimized_v2 runs at
        // 0.778, so this is as far as a repair calibrated on the catalogue is entitled to move it.
        IndicatorBounds.repair(inverted) mustBe Indicator.VolatilityRegimeDetection(22, VT.SMA(16))
      }

      "separate two lines that were a whisker apart, keeping the anchor and the orientation" in {
        // The same champion's crossover pair, at a ratio of 1.086.
        val noise = Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(35, -29, 1), VT.JMA(38, 19, 4))
        IndicatorBounds.repair(noise) mustBe Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(35, -29, 1), VT.JMA(42, 19, 4))
      }

      "repair an inverted pair from the slower side, leaving it inverted" in {
        val noise = Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(38, 19, 4), VT.JMA(35, -29, 1))
        IndicatorBounds.repair(noise) mustBe Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(42, 19, 4), VT.JMA(35, -29, 1))
      }

      "pull a gene that left its own range back into it" in {
        val out = Indicator.TrendChangeDetection(ValueSource.HLC3, VT.JMA(length = 240, phase = -400, power = 25))
        IndicatorBounds.repair(out) mustBe Indicator.TrendChangeDetection(ValueSource.HLC3, VT.JMA(100, -100, 10))
      }

      "leave a ratio broken rather than rewrite the gene the pair is anchored on" in {
        // A fast line at 95 wants a slow partner of at least 114 and jmaLength stops at 100, so no legal pair exists. The slow line goes
        // as far as its range allows, the fast line is left where the strategy put it, and the ratio stays broken at 1.05 - this is the JMA
        // ceiling surfacing as an unrepairable pair rather than as a clamp nobody sees.
        val unreachable = Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(95, 0, 1), VT.JMA(96, 0, 1))
        IndicatorBounds.repair(unreachable) mustBe Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(95, 0, 1), VT.JMA(100, 0, 1))
      }
    }

    "applied twice" should {
      "make no further change to anything the operators can produce" in {
        given Random = Random(31)
        val seeds    = catalogue.map(_._2)
        val result   = for
          mutator <- IndicatorMutator.make[IO]
          cross   <- IndicatorCrossover.make[IO]
          mutated <- (1 to 40).toList.traverse(_ => seeds.traverse(mutator.mutate(_, 1.0)))
          crossed <- (1 to 40).toList.traverse(_ => seeds.traverse(s => cross.cross(s, s)))
        yield mutated.flatten ++ crossed.flatten

        result.asserting { produced =>
          produced.filter(ind => IndicatorBounds.repair(ind) != ind) mustBe Nil
        }
      }
    }
  }

  /** Idempotence is too weak to be the operators' guarantee: an invalid indicator that `repair` cannot fix is a fixed point of it, so
    * `repair(repair(x)) == repair(x)` holds just as well for the failures as for the successes. The property that matters is that a valid
    * parent cannot produce an invalid child, which needs `isValid` rather than `repair` to state.
    */
  "IndicatorBounds.isValid" should {
    "hold for every strategy in the catalogue" in {
      catalogue.collect { case (name, ind) if !IndicatorBounds.isValid(ind) => name } mustBe Nil
    }

    "reject the failures repair was built for" in {
      IndicatorBounds.isValid(Indicator.VolatilityRegimeDetection(22, VT.SMA(6))) mustBe false
      IndicatorBounds.isValid(Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(35, -29, 1), VT.JMA(38, 19, 4))) mustBe false
      IndicatorBounds.isValid(Indicator.TrendChangeDetection(ValueSource.HLC3, VT.JMA(240, 0, 1))) mustBe false
    }

    "stay false after repairing a pair whose anchor has no legal partner" in {
      // The honest limit of `repair`, and the reason `isValid` exists as a separate question: a fast line at 95 wants a slow partner of at
      // least 114 against a ceiling of 100, and repair will not shorten the anchor to manufacture one.
      val stranded = Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(95, 0, 1), VT.JMA(96, 0, 1))
      IndicatorBounds.isValid(IndicatorBounds.repair(stranded)) mustBe false
    }

    "be restored by mutation, which is allowed to move the anchor" in {
      given Random = Random(8)
      val stranded = Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(95, 0, 1), VT.JMA(96, 0, 1))
      val result   = for
        mutator <- IndicatorMutator.make[IO]
        walked  <- mutator.mutate(stranded, 1.0)
      yield walked

      result.asserting(IndicatorBounds.isValid(_) mustBe true)
    }
  }

  "The operators" should {
    "preserve validity through a long run of mutation" in {
      given Random = Random(77)
      val result   = for
        mutator  <- IndicatorMutator.make[IO]
        lineages <- catalogue.map(_._2).traverse { seed =>
          (1 to 60).toList.foldLeft(IO.pure(List(seed)))((acc, _) =>
            acc.flatMap(walked => mutator.mutate(walked.head, 1.0).map(_ :: walked))
          )
        }
      yield lineages.flatten

      result.asserting(produced => produced.filterNot(IndicatorBounds.isValid) mustBe Nil)
    }

    "preserve validity through crossover of every compatible pair in the catalogue" in {
      given Random = Random(91)
      val pairs    = for
        (_, a) <- catalogue
        (_, b) <- catalogue
        if IndicatorCrossover.sameShape(a, b)
      yield (a, b)

      val result = for
        cross    <- IndicatorCrossover.make[IO]
        children <- (1 to 30).toList.traverse(_ => pairs.traverse((a, b) => cross.cross(a, b)))
      yield children.flatten

      result.asserting { produced =>
        produced must not be empty
        produced.filterNot(IndicatorBounds.isValid) mustBe Nil
      }
    }

    "preserve validity across a whole shuffled starting population" in {
      given Random = Random(64)
      val result   = for
        init <- IndicatorInitialiser.seeded[IO](catalogue.map(_._2))
        pop  <- init.initialisePopulation(TestStrategy.s6.indicator, 300, true)
      yield pop

      result.asserting(pop => pop.filterNot(IndicatorBounds.isValid) mustBe Vector.empty)
    }
  }

  "An IndicatorMutator" should {
    "hold a squeeze inside its ratio band however hard it is walked" in {
      given Random = Random(17)
      val squeeze  = Indicator.VolatilityRegimeDetection(atrLength = 20, smoothingType = VT.SMA(50))
      val result   = for
        mutator <- IndicatorMutator.make[IO]
        walked  <- (1 to 500).toList.foldLeft(IO.pure(squeeze))((acc, _) => acc.flatMap(mutator.mutate(_, 1.0)))
      yield walked

      result.asserting {
        case Indicator.VolatilityRegimeDetection(atr, VT.SMA(smoothing)) =>
          val ratio = smoothing.toDouble / atr
          ratio must be >= IndicatorBounds.volatilityRegime.valid.min
          ratio must be <= IndicatorBounds.volatilityRegime.valid.max
        case other => fail(s"mutation changed the shape of the indicator: $other")
      }
    }
  }

  "An IndicatorCrossover" should {
    "not land between two parents that each satisfy the ratio" in {
      given Random = Random(23)
      // One parent's squeeze is slow and the other's is fast; a per-gene weight can take the ATR from one and the smoothing from the other.
      val fast   = Indicator.VolatilityRegimeDetection(atrLength = 45, smoothingType = VT.SMA(95))
      val slow   = Indicator.VolatilityRegimeDetection(atrLength = 6, smoothingType = VT.SMA(14))
      val result = for
        cross    <- IndicatorCrossover.make[IO]
        children <- (1 to 200).toList.traverse(_ => cross.cross(fast, slow))
      yield children

      result.asserting { children =>
        val ratios = children.collect { case Indicator.VolatilityRegimeDetection(atr, VT.SMA(s)) => s.toDouble / atr }
        ratios must have size 200
        ratios.filter(_ < IndicatorBounds.volatilityRegime.valid.min) mustBe Nil
      }
    }

    "pair the faster line of each parent when the two parents are the other way round" in {
      given Random  = Random(4)
      val fastFirst = Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(20, 0, 1), VT.JMA(60, 0, 1))
      val slowFirst = Indicator.LinesCrossing(ValueSource.HLC3, VT.JMA(64, 0, 1), VT.JMA(22, 0, 1))
      val result    = for
        cross    <- IndicatorCrossover.make[IO]
        children <- (1 to 200).toList.traverse(_ => cross.cross(fastFirst, slowFirst))
      yield children

      result.asserting { children =>
        val ratios = children.collect { case Indicator.LinesCrossing(_, VT.JMA(l1, _, _), VT.JMA(l2, _, _)) =>
          math.max(l1, l2).toDouble / math.min(l1, l2)
        }
        ratios must have size 200
        ratios.filter(_ < IndicatorBounds.linesSeparation.valid.min) mustBe Nil
      }
    }
  }

  "IndicatorBounds.Relation.project" should {
    "be idempotent for every anchor and dependent in range" in {
      val relation = IndicatorBounds.volatilityRegime
      val range    = GeneBounds.maLength
      val broken   = for
        anchor    <- GeneBounds.atrLength.min to GeneBounds.atrLength.max
        dependent <- range.min to range.max
        once  = relation.project(anchor, dependent, range)
        twice = relation.project(anchor, once, range)
        if once != twice
      yield (anchor, dependent, once, twice)

      broken mustBe empty
    }

    "never move a dependent that already holds the ratio" in {
      val relation = IndicatorBounds.volatilityRegime
      val range    = GeneBounds.maLength
      val moved    = for
        anchor    <- GeneBounds.atrLength.min to GeneBounds.atrLength.max
        dependent <- range.min to range.max
        if relation.holds(anchor, dependent)
        if relation.project(anchor, dependent, range) != dependent
      yield (anchor, dependent)

      moved mustBe empty
    }

    "reach a legal ratio for every anchor the mutator is allowed to use" in {
      // The claim `repair` cannot make on its own: inside `feasibleAnchor` a legal dependent always exists, which is why mutation holds the
      // anchor there rather than in its own full range.
      val relation = IndicatorBounds.linesSeparation
      val range    = GeneBounds.jmaLength
      val stranded = for
        anchor <- relation.feasibleAnchor(range, range).min to relation.feasibleAnchor(range, range).max
        if !relation.holds(anchor, relation.project(anchor, range.min, range))
      yield anchor

      stranded mustBe empty
    }

    "narrow the anchor only where the dependent's range cannot follow" in {
      val lines = IndicatorBounds.linesSeparation.feasibleAnchor(GeneBounds.jmaLength, GeneBounds.jmaLength)
      lines mustBe GeneBounds.IntRange(5, 83)

      val squeeze = IndicatorBounds.volatilityRegime.feasibleAnchor(GeneBounds.atrLength, GeneBounds.maLength)
      squeeze mustBe GeneBounds.atrLength
    }
  }
}
