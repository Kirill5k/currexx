package currexx.backtest.optimizer

import cats.effect.IO
import cats.syntax.apply.*
import cats.syntax.traverse.*
import currexx.backtest.TestStrategy
import currexx.backtest.optimizer.indicator.{IndicatorDistance, IndicatorSearchOperators}
import currexx.backtest.optimizer.strategy.{
  RuleList,
  RuleParameter,
  RuleParameterBounds,
  RuleParameterPath,
  RulePath,
  RuleTarget,
  TradeStrategySearchOperators
}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Direction, Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.concurrent.duration.*
import scala.util.Random

class OptimisationSearchOperatorsSpec extends IOWordSpec {
  private val base = TestStrategy(
    Indicator.TrendChangeDetection(ValueSource.Close, VT.JMA(20, 0, 1)),
    TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.TrendActiveFor(3.hours))), Nil)
  )
  private val changedRules = base.rules.copy(openRules = List(Rule(TradeAction.OpenLong, Rule.Condition.TrendActiveFor(9.hours))))
  private val changed   = base.copy(indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.JMA(40, 10, 2)), rules = changedRules)
  private val ageBounds = RuleParameterBounds(
    durations = Map(RuleTarget.TrendAge -> RuleParameterBounds.DurationRange(0.hours, 24.hours, 1.hour))
  )

  private def space(
      target: OptimisationTarget,
      template: TestStrategy = base,
      fixed: Set[Indicator] = Set.empty,
      bounds: RuleParameterBounds = RuleParameterBounds()
  ): OptimisationSearchSpace =
    OptimisationSearchSpace
      .forRound(
        OptimisationRound(
          "operators",
          template,
          OptimisationRounds.gaParameters,
          ScoringFunction.Robust(),
          target = target,
          fixedIndicators = fixed,
          ruleBounds = bounds
        )
      )
      .fold(error => fail(error.getMessage), identity)

  "Complete candidate search operators" should {
    "preserve the indicator-only population, variation, distance and random sequence" in {
      val searchSpace                                                                    = space(OptimisationTarget.Indicator)
      def legacy: IO[(Vector[TestStrategy], TestStrategy, TestStrategy, Double, Double)] = {
        given Random = Random(137)
        for
          operators <- IndicatorSearchOperators
            .fromProjectedSeeds[IO](searchSpace.indicatorSpace.get, List(Indicator.compositeAnyOf(changed.indicator)))
          population <- operators.initialiser.initialisePopulation(base.indicator, 30, shuffle = true)
          mutated    <- operators.mutator.mutate(population.last, 0.4)
          crossed    <- operators.crossover.cross(population.head, mutated, 0.7)
          distance   <- IO.fromEither(IndicatorDistance.make(searchSpace.indicatorSpace.get).between(mutated, crossed))
          next       <- IO(summon[Random].nextDouble())
        yield (
          population.map(i => base.copy(indicator = i)),
          base.copy(indicator = mutated),
          base.copy(indicator = crossed),
          distance,
          next
        )
      }
      def combined: IO[(Vector[TestStrategy], TestStrategy, TestStrategy, Double, Double)] = {
        given Random = Random(137)
        for
          operators  <- OptimisationSearchOperators.make[IO](searchSpace, List(changed))
          population <- operators.initialiser.initialisePopulation(base, 30, shuffle = true)
          mutated    <- operators.mutator.mutate(population.last, 0.4)
          crossed    <- operators.crossover.cross(population.head, mutated, 0.7)
          distance   <- IO.fromEither(operators.distance.between(mutated, crossed))
          next       <- IO(summon[Random].nextDouble())
        yield (population, mutated, crossed, distance, next)
      }
      (legacy, combined).tupled.asserting { case (expected, actual) =>
        actual mustBe expected
        actual._1.sortBy(_.toString) mustBe expected._1.sortBy(_.indicator.toString)
      }
    }

    "reject invalid probabilities and undersized populations without drawing randomness in every mode" in {
      class CountedRandom extends Random(53) {
        var draws                             = 0
        override def nextDouble(): Double     = { draws += 1; super.nextDouble() }
        override def nextInt(bound: Int): Int = { draws += 1; super.nextInt(bound) }
        override def nextBoolean(): Boolean   = { draws += 1; super.nextBoolean() }
        override def nextGaussian(): Double   = { draws += 1; super.nextGaussian() }
      }
      val modes = List(
        space(OptimisationTarget.Indicator),
        space(OptimisationTarget.Strategy, bounds = ageBounds),
        space(OptimisationTarget.Both, bounds = ageBounds),
        space(OptimisationTarget.Indicator, fixed = Set(base.indicator))
      )
      val invalidProbabilities = List(-0.1, 1.1, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)
      modes
        .traverse { searchSpace =>
          val random        = new CountedRandom
          given Random      = random
          val distinctCount = searchSpace.projectSeeds(List(changed)).fold(error => fail(error.getMessage), _.size + 1)
          for
            operators     <- OptimisationSearchOperators.make[IO](searchSpace, List(changed, changed))
            probabilities <- invalidProbabilities.traverse { probability =>
              for
                mutation  <- operators.mutator.mutate(base, probability).attempt
                crossover <- operators.crossover.cross(base, changed, probability).attempt
              yield List(mutation, crossover)
            }
            populations <- List(-1, 0, distinctCount - 1).distinct.traverse { size =>
              operators.initialiser.initialisePopulation(base, size, shuffle = true).attempt
            }
            drawsBeforeValid <- IO(random.draws)
            exact            <- operators.initialiser.initialisePopulation(base, distinctCount, shuffle = true)
            drawsAfterValid  <- IO(random.draws)
          yield (searchSpace, probabilities.flatten, populations, exact, drawsBeforeValid, drawsAfterValid)
        }
        .asserting { outcomes =>
          outcomes.foreach { (searchSpace, probabilities, populations, exact, before, after) =>
            probabilities.foreach(_.left.map(_.getMessage) mustBe Left("Probability must be finite and between zero and one"))
            populations.foreach(_.left.map(_.getMessage) mustBe Left("Population size must be positive and hold all distinct seeds"))
            exact mustBe (base +: searchSpace.projectSeeds(List(changed)).toOption.get).toVector
            before mustBe 0
            after mustBe 0
          }
          succeed
        }
    }

    "initialise a single joint population while retaining complete seed pairings" in {
      given Random      = Random(83)
      val searchSpace   = space(OptimisationTarget.Both, bounds = ageBounds)
      val ruleSeed      = base.copy(rules = changedRules)
      val indicatorSeed = changed.copy(rules = base.rules)
      val result        = for
        operators <- OptimisationSearchOperators.make[IO](searchSpace, List(ruleSeed, indicatorSeed, changed))
        refining  <- operators.initialiser.initialisePopulation(base, 40, shuffle = false)
        exploring <- operators.initialiser.initialisePopulation(base, 100, shuffle = true)
        tooSmall  <- operators.initialiser.initialisePopulation(base, 3, shuffle = true).attempt
      yield (refining, exploring, tooSmall)

      result.asserting { case (refining, exploring, tooSmall) =>
        refining must have size 40
        exploring must have size 100
        refining.take(4) mustBe Vector(base, ruleSeed, indicatorSeed, changed)
        exploring.take(4) mustBe Vector(base, ruleSeed, indicatorSeed, changed)
        exploring.exists(candidate => candidate.indicator != base.indicator && candidate.rules != base.rules) mustBe true
        exploring.foreach(candidate => searchSpace.canonicalise(candidate) mustBe Right(candidate))
        tooSmall.left.map(_.getMessage) mustBe Left("Population size must be positive and hold all distinct seeds")
      }
    }

    "broaden joint numeric neighbours with the radius while leaving categories fixed" in {
      given Random   = Random(731)
      val categories = List.fill(8)(Rule.Condition.TrendIs(Direction.Upward))
      val template   = TestStrategy(
        Indicator
          .compositeAnyOf(base.indicator, Indicator.ValueTracking(ValueRole.Velocity, ValueSource.Close, VT.KalmanVelocity(0.5, 0.1))),
        TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.AllOf(categories :+ Rule.Condition.VelocityIsBelow(1.0)))), Nil)
      )
      val root   = RulePath(RuleList.Open, 0)
      val bounds = RuleParameterBounds(
        numeric = Map(
          RuleTarget
            .At(RuleParameterPath(root.child(8), RuleParameter.Threshold), template.rules) -> RuleParameterBounds.DoubleRange(0, 2, 0.01)
        )
      )
      val searchSpace                                           = space(OptimisationTarget.Both, template, Set(template.indicator), bounds)
      def values(candidate: TestStrategy): List[Rule.Condition] = candidate.rules.openRules.head.conditions match
        case Rule.Condition.AllOf(conditions) => conditions
        case _                                => fail("Expected the fixed condition tree")
      def numericChange(candidate: TestStrategy): Double =
        values(candidate)
          .collectFirst { case Rule.Condition.VelocityIsBelow(threshold) => math.abs(threshold - 1.0) }
          .getOrElse(fail("Expected the numeric leaf"))
      def average(candidates: Vector[TestStrategy])(measure: TestStrategy => Double): Double =
        candidates.map(measure).sum / candidates.size

      val result = for
        operators <- OptimisationSearchOperators.make[IO](searchSpace)
        refining  <- operators.initialiser.initialisePopulation(template, 1000, shuffle = false)
        exploring <- operators.initialiser.initialisePopulation(template, 1000, shuffle = true)
      yield (refining.slice(300, 900), exploring.slice(150, 700))

      result.asserting { case (nearby, exploratory) =>
        (nearby ++ exploratory).foreach(candidate => values(candidate).take(8) mustBe categories)
        val radii =
          Vector.tabulate(3)(radius => exploratory.zipWithIndex.collect { case (candidate, index) if index % 3 == radius => candidate })
        val numericMeans = radii.map(average(_)(numericChange))
        numericMeans(0) must be < 0.3
        numericMeans(1) must be > numericMeans(0)
        numericMeans(2) must be > numericMeans(1)
        (nearby ++ exploratory).foreach(candidate => searchSpace.canonicalise(candidate) mustBe Right(candidate))
        succeed
      }
    }

    "restore the fixed indicator in strategy-only initialisation, mutation and crossover" in {
      given Random    = Random(29)
      val searchSpace = space(OptimisationTarget.Strategy, bounds = ageBounds)
      val result      = for
        operators  <- OptimisationSearchOperators.make[IO](searchSpace, List(changed))
        population <- operators.initialiser.initialisePopulation(changed, 20, shuffle = true)
        mutations  <- population.traverse(operators.mutator.mutate(_, 1.0))
        children   <- mutations.traverse(operators.crossover.cross(_, changed, 1.0))
      yield population ++ mutations ++ children
      result.asserting { candidates =>
        candidates.forall(_.indicator == base.indicator) mustBe true
        candidates.map(_.rules).distinct.size must be > 1
      }
    }

    "make exactly one joint crossover probability decision and preserve skipped parents" in {
      class CountedRandom extends Random(0) {
        var doubles: Int                      = 0
        override def nextDouble(): Double     = { doubles += 1; 0.4 }
        override def nextInt(bound: Int): Int = 0
        override def nextBoolean(): Boolean   = true
      }
      def run(probability: Option[Double]): IO[(TestStrategy, Int)] = {
        val random   = new CountedRandom
        given Random = random
        for
          operators <- OptimisationSearchOperators.make[IO](space(OptimisationTarget.Both, bounds = ageBounds))
          child     <- probability.fold(operators.crossover.cross(base, changed))(p => operators.crossover.cross(base, changed, p))
        yield (child, random.doubles)
      }
      (run(None), run(Some(1.0)), run(Some(0.0))).tupled.asserting { case ((ungated, before), (gated, after), (skipped, skippedDraws)) =>
        gated mustBe ungated
        after mustBe before + 1
        skipped mustBe base
        skippedDraws mustBe 1
      }
    }

    "weight linked rule groups once when combining indicator and duration distances" in {
      given Random                                          = Random(8)
      def repeatedRules(age: FiniteDuration): TradeStrategy =
        TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.AllOf(List.fill(3)(Rule.Condition.TrendActiveFor(age))))), Nil)
      val first       = base.copy(rules = repeatedRules(3.hours))
      val second      = changed.copy(rules = repeatedRules(9.hours))
      val searchSpace = space(OptimisationTarget.Both, first, bounds = ageBounds)
      val result      = for
        operators <- OptimisationSearchOperators.make[IO](searchSpace)
        indicator <- IO.fromEither(IndicatorDistance.make(searchSpace.indicatorSpace.get).between(first.indicator, second.indicator))
        rules     <- IO.fromEither(TradeStrategySearchOperators.distance(searchSpace.strategySpace.get).between(first.rules, second.rules))
        joint     <- IO.fromEither(operators.distance.between(first, second))
      yield (indicator, rules, joint)
      result.asserting { case (indicator, rules, joint) =>
        searchSpace.indicatorParameterCount mustBe 3
        searchSpace.strategyParameterCount mustBe 1
        rules mustBe 0.25
        joint mustBe math.sqrt((3 * indicator * indicator + rules * rules) / 4) +- 1e-12
      }
    }

    "vary only indicators in Both when the rules have no tunable parameters" in {
      val noNumericRules = base.copy(rules = TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil))
      def run(template: TestStrategy, target: OptimisationTarget): IO[(Vector[TestStrategy], Double, Double)] = {
        given Random    = Random(491)
        val searchSpace = space(target, template)
        val sibling     = template.copy(indicator = changed.indicator)
        for
          operators  <- OptimisationSearchOperators.make[IO](searchSpace, List(sibling))
          population <- operators.initialiser.initialisePopulation(template, 30, shuffle = true)
          mutations  <- population.traverse(operators.mutator.mutate(_, 0.7))
          children   <- mutations.traverse(operators.crossover.cross(_, sibling, 0.6))
          distance   <- IO.fromEither(operators.distance.between(template, sibling))
          next       <- IO(summon[Random].nextDouble())
        yield (population ++ mutations ++ children, distance, next)
      }
      List(base, noNumericRules)
        .traverse { template =>
          (run(template, OptimisationTarget.Indicator), run(template, OptimisationTarget.Both)).tupled.map { (indicator, joint) =>
            (template, indicator, joint)
          }
        }
        .asserting { outcomes =>
          outcomes.foreach { (template, indicator, joint) =>
            space(OptimisationTarget.Both, template).strategyParameterCount mustBe 0
            joint._1 mustBe indicator._1
            joint._1.forall(_.rules == template.rules) mustBe true
            joint._1.map(_.indicator).distinct.size must be > 1
            joint._2 mustBe indicator._2 +- 1e-12
            joint._3 mustBe indicator._3
          }
          succeed
        }
    }

    "return zero distance and preserve a Both candidate when every parameter is fixed" in {
      given Random    = Random(307)
      val searchSpace = space(OptimisationTarget.Both, fixed = Set(base.indicator))
      val result      = for
        operators  <- OptimisationSearchOperators.make[IO](searchSpace, List(changed))
        population <- operators.initialiser.initialisePopulation(base, 12, shuffle = true)
        mutated    <- operators.mutator.mutate(base, 1.0)
        crossed    <- operators.crossover.cross(base, base, 1.0)
        distance   <- IO.fromEither(operators.distance.between(base, base))
      yield (population, mutated, crossed, distance)
      result.asserting { (population, mutated, crossed, distance) =>
        searchSpace.parameterCount mustBe 0
        population mustBe Vector.fill(12)(base)
        mutated mustBe base
        crossed mustBe base
        distance mustBe 0.0
      }
    }

    "ignore frozen blocks in distance and keep entirely fixed candidates unchanged" in {
      given Random         = Random(11)
      val frozenIndicators = space(OptimisationTarget.Both, fixed = Set(base.indicator), bounds = ageBounds)
      val noParameters     = base.copy(rules = TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil))
      val fixed            = space(OptimisationTarget.Indicator, noParameters, Set(base.indicator))
      val result           = for
        rulesOnly <- OptimisationSearchOperators.make[IO](frozenIndicators)
        distance  <- IO.fromEither(rulesOnly.distance.between(base, changed))
        expected  <- IO.fromEither(
          TradeStrategySearchOperators.distance(frozenIndicators.strategySpace.get).between(base.rules, changed.rules)
        )
        operators <- OptimisationSearchOperators.make[IO](fixed)
        initial   <- operators.initialiser.initialisePopulation(noParameters, 12, shuffle = true)
        mutated   <- operators.mutator.mutate(noParameters, 1.0)
        crossed   <- operators.crossover.cross(noParameters, noParameters, 1.0)
        zero      <- IO.fromEither(operators.distance.between(noParameters, noParameters))
      yield (distance, expected, initial, mutated, crossed, zero)
      result.asserting { case (distance, expected, initial, mutated, crossed, zero) =>
        distance mustBe expected
        fixed.parameterCount mustBe 0
        initial mustBe Vector.fill(12)(noParameters)
        mutated mustBe noParameters
        crossed mustBe noParameters
        zero mustBe 0.0
      }
    }
  }
}
