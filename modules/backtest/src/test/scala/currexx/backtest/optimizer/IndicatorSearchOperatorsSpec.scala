package currexx.backtest.optimizer

import cats.effect.IO
import cats.syntax.traverse.*
import currexx.backtest.TestStrategy
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class IndicatorSearchOperatorsSpec extends IOWordSpec {

  private val s10 = TestStrategy.s10
  private val s5  = TestStrategy.s5_optimized_v2

  "Strategy-aware indicator search operators" should {
    "preserve s10's raw-price exit in clones and random immigrants with either initialisation mix" in {
      given Random = Random(42)
      val space    = searchSpace(s10)

      val result = for
        operators <- IndicatorSearchOperators.make[IO](space)
        refining  <- operators.initialiser.initialisePopulation(s10.indicator, 100, shuffle = false)
        exploring <- operators.initialiser.initialisePopulation(s10.indicator, 100, shuffle = true)
      yield List(refining, exploring)

      result.asserting { populations =>
        populations.foreach { population =>
          population must have size 100
          population must contain(s10.indicator)
          population.distinct.size must be > 20
          population.foreach { candidate =>
            tracker(candidate, ValueRole.Price) mustBe VT.SMA(1)
            space.project(candidate).exists(_.exists(IndicatorBounds.isValid)) mustBe true
          }
        }
        succeed
      }
    }

    "leave raw price untouched by both zero-probability bounds repair and full mutation" in {
      given Random = Random(73)
      val space    = searchSpace(s10)

      val result = for
        operators <- IndicatorSearchOperators.make[IO](space)
        unchanged <- operators.mutator.mutate(s10.indicator, 0.0)
        mutated   <- List.fill(40)(s10.indicator).traverse(operators.mutator.mutate(_, 1.0))
      yield (unchanged, mutated)

      result.asserting { case (unchanged, mutated) =>
        unchanged mustBe s10.indicator
        mutated.distinct.size must be > 20
        mutated.foreach { candidate =>
          tracker(candidate, ValueRole.Price) mustBe VT.SMA(1)
          space.project(candidate).exists(_.exists(IndicatorBounds.isValid)) mustBe true
        }
        succeed
      }
    }

    "canonicalise fixed genes in both crossover parents even when crossover is skipped" in {
      given Random = Random(117)
      val space    = searchSpace(s10)
      val sibling  = replace(s10.indicator) {
        case Indicator.ValueTracking(ValueRole.Price, source, _) => Indicator.ValueTracking(ValueRole.Price, source, VT.SMA(67))
        case Indicator.TrendChangeDetection(source, _)           => Indicator.TrendChangeDetection(source, VT.JMA(50, -6, 1))
      }

      val result = for
        operators <- IndicatorSearchOperators.make[IO](space)
        forward   <- operators.crossover.cross(s10.indicator, sibling, 0.0)
        reverse   <- operators.crossover.cross(sibling, s10.indicator, 0.0)
        crossed   <- List.fill(30)((s10.indicator, sibling)).traverse { case (first, second) =>
          for
            child1 <- operators.crossover.cross(first, second, 1.0)
            child2 <- operators.crossover.cross(second, first, 1.0)
          yield List(child1, child2)
        }
      yield (forward, reverse, crossed.flatten)

      result.asserting { case (forward, reverse, crossed) =>
        forward mustBe s10.indicator
        space.canonicalise(sibling) mustBe Right(reverse)
        crossed.distinct.size must be > 2
        (forward :: reverse :: crossed).foreach { candidate =>
          tracker(candidate, ValueRole.Price) mustBe VT.SMA(1)
          space.project(candidate).exists(_.exists(IndicatorBounds.isValid)) mustBe true
        }
        succeed
      }
    }

    "canonicalise extra seeds before cloning them and retain their searchable parameters" in {
      given Random = Random(32)
      val space    = searchSpace(s10)
      val sibling  = replace(s10.indicator) {
        case Indicator.ValueTracking(ValueRole.Price, source, _) => Indicator.ValueTracking(ValueRole.Price, source, VT.SMA(67))
        case Indicator.TrendChangeDetection(source, _)           => Indicator.TrendChangeDetection(source, VT.JMA(50, -6, 1))
      }

      val result = for
        operators <- IndicatorSearchOperators.make[IO](space, List(sibling))
        refining  <- operators.initialiser.initialisePopulation(s10.indicator, 100, shuffle = false)
        exploring <- operators.initialiser.initialisePopulation(s10.indicator, 100, shuffle = true)
      yield List(refining, exploring)

      result.asserting { populations =>
        populations.foreach { population =>
          space.canonicalise(sibling).exists(population.contains) mustBe true
          population must not contain sibling
          population.foreach(candidate => tracker(candidate, ValueRole.Price) mustBe VT.SMA(1))
        }
        succeed
      }
    }

    "give no extra clone share to seeds that differ only in an inactive tracker" in {
      val space   = searchSpace(s5)
      val aliases = List(12, 25, 40).map { length =>
        replace(s5.indicator) { case Indicator.ValueTracking(ValueRole.Momentum, source, _) =>
          Indicator.ValueTracking(ValueRole.Momentum, source, VT.RSX(length))
        }
      }

      def initialise(extraSeeds: List[Indicator]): IO[Vector[Indicator]] = {
        given Random = Random(101)
        for
          operators  <- IndicatorSearchOperators.make[IO](space, extraSeeds)
          population <- operators.initialiser.initialisePopulation(s5.indicator, 100, shuffle = true)
        yield population
      }

      val result = for
        withoutAliases <- initialise(Nil)
        withAliases    <- initialise(aliases)
      yield (withoutAliases, withAliases)

      result.asserting { case (withoutAliases, withAliases) =>
        withAliases mustBe withoutAliases
      }
    }

    "pin s5's unused momentum tracker while its entry and exit parameters remain searchable" in {
      given Random = Random(65)
      val space    = searchSpace(s5)

      val result = for
        operators  <- IndicatorSearchOperators.make[IO](space)
        population <- operators.initialiser.initialisePopulation(s5.indicator, 100, shuffle = true)
        mutated    <- population.toList.traverse(operators.mutator.mutate(_, 1.0))
        crossed    <- mutated.traverse(operators.crossover.cross(_, s5.indicator, 1.0))
      yield (population.toList, mutated, crossed)

      result.asserting { case (population, mutated, crossed) =>
        List(population, mutated, crossed).foreach { candidates =>
          candidates.distinct.size must be > 20
          candidates.foreach { candidate =>
            tracker(candidate, ValueRole.Momentum) mustBe VT.RSX(8)
            space.project(candidate).exists(_.exists(IndicatorBounds.isValid)) mustBe true
          }
          candidates.flatMap(leaves).collect { case threshold: Indicator.ThresholdCrossing => threshold }.distinct.size must be > 10
        }
        succeed
      }
    }

    "preserve an explicitly frozen nested subtree through every operator" in {
      given Random = Random(84)
      val original = leaves(s10.indicator)
      val frozen   = Indicator.compositeAnyOf(original(0), original(1))
      val strategy = s10.copy(indicator = Indicator.compositeAnyOf(frozen, original.drop(2)*))
      val space    = searchSpace(strategy, fixedIndicators = Set(frozen))

      val result = for
        operators  <- IndicatorSearchOperators.make[IO](space)
        population <- operators.initialiser.initialisePopulation(strategy.indicator, 80, shuffle = true)
        mutated    <- population.toList.traverse(operators.mutator.mutate(_, 1.0))
        crossed    <- mutated.traverse(operators.crossover.cross(_, strategy.indicator, 1.0))
      yield population.toList ++ mutated ++ crossed

      result.asserting { candidates =>
        candidates.distinct.size must be > 20
        candidates.foreach {
          case Indicator.Composite(children, _) => children.head mustBe frozen
          case other                            => fail(s"lost the composite shape: $other")
        }
        succeed
      }
    }

    "return the template when all indicators are explicitly fixed" in {
      given Random = Random(8)
      val space    = searchSpace(s10, fixedIndicators = Set(s10.indicator))
      val sibling  = replace(s10.indicator) {
        case Indicator.ValueTracking(ValueRole.Price, source, _) => Indicator.ValueTracking(ValueRole.Price, source, VT.SMA(67))
        case Indicator.TrendChangeDetection(source, _)           => Indicator.TrendChangeDetection(source, VT.JMA(50, -6, 1))
      }

      val result = for
        operators  <- IndicatorSearchOperators.make[IO](space, List(sibling))
        population <- operators.initialiser.initialisePopulation(sibling, 30, shuffle = true)
        mutated    <- operators.mutator.mutate(sibling, 1.0)
        crossed    <- operators.crossover.cross(sibling, s10.indicator, 1.0)
        copied     <- operators.crossover.cross(sibling, s10.indicator, 0.0)
      yield (population, mutated, crossed, copied)

      result.asserting { case (population, mutated, crossed, copied) =>
        population mustBe Vector.fill(30)(s10.indicator)
        mutated mustBe s10.indicator
        crossed mustBe s10.indicator
        copied mustBe s10.indicator
      }
    }

    "reject incompatible operator inputs even when their operation probability is zero" in {
      given Random = Random(7)
      val space    = searchSpace(s10)
      val invalid  = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(20))

      val result = for
        operators         <- IndicatorSearchOperators.make[IO](space, List(invalid))
        population        <- operators.initialiser.initialisePopulation(s10.indicator, 20, shuffle = true)
        badInitialisation <- operators.initialiser.initialisePopulation(invalid, 20, shuffle = true).attempt
        badMutation       <- operators.mutator.mutate(invalid, 0.0).attempt
        badFirstParent    <- operators.crossover.cross(invalid, s10.indicator, 0.0).attempt
        badSecondParent   <- operators.crossover.cross(s10.indicator, invalid, 0.0).attempt
        badCross          <- operators.crossover.cross(s10.indicator, invalid, 1.0).attempt
      yield (population, List(badInitialisation, badMutation, badFirstParent, badSecondParent, badCross))

      result.asserting { case (population, errors) =>
        population must have size 20
        population.forall(space.accepts) mustBe true
        errors.foreach(_.isLeft mustBe true)
        succeed
      }
    }
  }

  private def searchSpace(strategy: TestStrategy, fixedIndicators: Set[Indicator] = Set.empty): IndicatorSearchSpace =
    IndicatorSearchSpace
      .forStrategy(strategy, fixedIndicators)
      .fold(error => fail(s"expected a valid search space for the test strategy: ${error.getMessage}"), identity)

  private def leaves(indicator: Indicator): List[Indicator] = indicator match
    case Indicator.Composite(children, _) => children.toList.flatMap(leaves)
    case leaf                             => List(leaf)

  private def tracker(indicator: Indicator, role: ValueRole): VT =
    leaves(indicator)
      .collectFirst { case Indicator.ValueTracking(`role`, _, transformation) => transformation }
      .getOrElse(fail(s"missing $role tracker in $indicator"))

  private def replace(indicator: Indicator)(change: PartialFunction[Indicator, Indicator]): Indicator = indicator match
    case Indicator.Composite(children, combinator) => Indicator.Composite(children.map(replace(_)(change)), combinator)
    case leaf                                      => change.applyOrElse(leaf, identity[Indicator])
}
