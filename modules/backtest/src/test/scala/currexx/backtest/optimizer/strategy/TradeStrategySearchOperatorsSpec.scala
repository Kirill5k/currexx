package currexx.backtest.optimizer.strategy

import cats.effect.IO
import cats.syntax.traverse.*
import currexx.algorithms.Population
import currexx.algorithms.operators.Initialiser
import currexx.backtest.TestStrategy
import currexx.backtest.optimizer.{
  OptimisationRound,
  OptimisationRounds,
  OptimisationSearchOperators,
  OptimisationSearchSpace,
  OptimisationTarget,
  ScoringFunction
}
import currexx.core.trade.{Rule, TradeStrategy}
import currexx.domain.signal.ValueRole
import kirill5k.common.cats.test.IOWordSpec

import scala.concurrent.duration.*
import scala.util.Random

class TradeStrategySearchOperatorsSpec extends IOWordSpec {
  import Rule.Condition as C
  import RuleParameterBounds.{DoubleRange, DurationRange}
  import TradeStrategyFixtures.*

  private class CountingRandom(seed: Long) extends Random(seed):
    var draws                             = 0
    override def nextDouble(): Double     = { draws += 1; super.nextDouble() }
    override def nextInt(bound: Int): Int = { draws += 1; super.nextInt(bound) }
    override def nextBoolean(): Boolean   = { draws += 1; super.nextBoolean() }
    override def nextGaussian(): Double   = { draws += 1; super.nextGaussian() }

  private def initialiser(base: TradeStrategy, domains: RuleParameterBounds, seeds: List[TradeStrategy] = Nil)(using
      Random
  ): IO[Initialiser[IO, TradeStrategy]] =
    for
      search <- IO.fromEither(
        OptimisationSearchSpace.forRound(
          OptimisationRound(
            "rules-test",
            TestStrategy(indicator, base),
            OptimisationRounds.gaParameters,
            ScoringFunction.Robust(),
            target = OptimisationTarget.Strategy,
            ruleBounds = domains
          )
        )
      )
      operators <- OptimisationSearchOperators.make[IO](search, seeds.map(TestStrategy(indicator, _)))
    yield new Initialiser[IO, TradeStrategy] {
      override def initialisePopulation(seed: TradeStrategy, size: Int, shuffle: Boolean): IO[Population[TradeStrategy]] =
        operators.initialiser.initialisePopulation(TestStrategy(indicator, seed), size, shuffle).map(_.map(_.rules))
    }

  "Trade strategy operators" should {
    "sample numeric fields while retaining every categorical value and the rule tree" in {
      given Random = Random(42)
      val result   = List.fill(300)(template).traverse(TradeStrategySearchOperators.randomise[IO](space, _))
      result.asserting { candidates =>
        candidates.distinct.size must be > 250
        candidates.foreach(candidate => space.validate(candidate) mustBe Right(candidate))
        val leaves         = candidates.map(RuleTree.leaves(_).map(_._2))
        val numericIndices = Set(2, 10, 11, 13, 18, 19)
        conditions.indices.foreach { index =>
          if (numericIndices.contains(index)) leaves.map(_(index)).distinct.size must be > 1
          else leaves.map(_(index)).distinct mustBe List(conditions(index))
        }
        val valueConditions = leaves.map(_(18)).collect { case value: C.ValueIs => value }
        valueConditions.map(_.role).toSet mustBe Set(ValueRole.Momentum)
        valueConditions.map(_.operator).toSet mustBe Set(Rule.Operator.GreaterThan)
        succeed
      }
    }

    "retain the base and every distinct seed in both population mixes" in {
      given Random = Random(87)
      val sibling  = changed(19, C.PriceMovedAgainstEntry(5.0))
      val result   = for
        populationBuilder <- initialiser(template, bounds, List(sibling, sibling))
        refining          <- populationBuilder.initialisePopulation(template, 80, shuffle = false)
        exploring         <- populationBuilder.initialisePopulation(template, 80, shuffle = true)
        invalid           <- populationBuilder.initialisePopulation(template, 1, shuffle = true).attempt
      yield (List(refining, exploring), invalid)
      result.asserting { case (populations, invalid) =>
        populations.foreach { population =>
          population must have size 80
          population must contain(template)
          population must contain(sibling)
          population.distinct.size must be > 30
          population.foreach(candidate => space.validate(candidate).isRight mustBe true)
        }
        invalid.isLeft mustBe true
        succeed
      }
    }

    "sample, mutate and cross each linked coordinate once and write it to every matching leaf" in {
      given Random = Random(105)
      val base     = mirrored()
      val local    = TradeStrategySearchSpace.forStrategy(base, indicator, linkedBounds).toOption.get
      val seed     = mirrored(5.hours, 24.hours, 6)
      val result   = for
        populationBuilder <- initialiser(base, linkedBounds, List(seed))
        crossover         <- IO.pure(TradeStrategySearchOperators.crossover[IO](local))
        mutator           <- IO.pure(TradeStrategySearchOperators.mutator[IO](local))
        population        <- populationBuilder.initialisePopulation(base, 100, shuffle = true)
        mutations         <- population.traverse(mutator.mutate(_, 1.0))
        children          <- population.zip(mutations).traverse((a, b) => crossover.cross(a, b))
      yield (population, mutations ++ children)
      result.asserting { case (population, changed) =>
        population.take(2) mustBe Vector(base, seed)
        local.parameterCount mustBe 3
        (population ++ changed).foreach { candidate =>
          local.validate(candidate) mustBe Right(candidate)
          val leaves = RuleTree.leaves(candidate).map(_._2)
          val ages   = leaves.collect { case C.TrendActiveFor(value) => value }
          val times  = leaves.collect { case C.PositionOpenFor(value) => value }
          val stops  = leaves.collect { case C.PriceMovedAgainstEntry(value) => value }
          ages.size mustBe 2
          times.size mustBe 2
          stops.size mustBe 2
          ages.distinct.size mustBe 1
          times.distinct.size mustBe 1
          stops.distinct.size mustBe 1
        }
        population.distinct.size must be > 30
      }
    }

    "draw once per linked group instead of once per occurrence" in {
      var decisions = 0
      var samples   = 0
      var gaussians = 0
      given Random  = new Random(1):
        override def nextDouble(): Double     = { decisions += 1; 0.5 }
        override def nextInt(bound: Int): Int = { samples += 1; bound / 2 }
        override def nextGaussian(): Double   = { gaussians += 1; 1.0 }
      val base   = mirrored()
      val local  = TradeStrategySearchSpace.forStrategy(base, indicator, linkedBounds).toOption.get
      val result = for
        crossover <- IO.pure(TradeStrategySearchOperators.crossover[IO](local))
        mutator   <- IO.pure(TradeStrategySearchOperators.mutator[IO](local))
        sampled   <- TradeStrategySearchOperators.randomise[IO](local, base)
        mutated   <- mutator.mutate(base, 1.0)
        child     <- crossover.cross(base, mirrored(6.hours, 24.hours, 6.0))
      yield List(sampled, mutated, child)
      result.asserting { candidates =>
        samples mustBe 3
        gaussians mustBe 3
        decisions mustBe 6
        candidates.foreach(candidate => local.validate(candidate).isRight mustBe true)
        succeed
      }
    }

    "keep the numeric Gaussian scale for nearby parameters" in {
      val root     = RulePath(RuleList.Open, 0)
      given Random = new Random(1):
        override def nextGaussian(): Double = 1.0
      val base  = single(C.VelocityIsBelow(0.5))
      val local = TradeStrategySearchSpace
        .forStrategy(
          base,
          indicator,
          RuleParameterBounds(
            numeric = Map(RuleTarget.At(RuleParameterPath(root, RuleParameter.Threshold), base) -> DoubleRange(0, 1, 0.01))
          )
        )
        .toOption
        .get
      List(1.0, 3.0, 6.0).traverse(TradeStrategySearchOperators.nearby[IO](local, base, _)).asserting { candidates =>
        candidates mustBe List(0.6, 0.8, 1.0).map(value => single(C.VelocityIsBelow(value)))
      }
    }

    "defer nearby sampling and consume no random draws at radius zero or for fixed defaults" in {
      given random: CountingRandom = new CountingRandom(43)
      val fixed                    = TradeStrategySearchSpace.forStrategy(template, indicator, RuleParameterBounds()).toOption.get
      val pending                  = TradeStrategySearchOperators.nearby[IO](space, template, 1)
      random.draws mustBe 0
      val result = for
        zero        <- TradeStrategySearchOperators.nearby[IO](space, template, 0)
        fixedNearby <- TradeStrategySearchOperators.nearby[IO](fixed, template, 6)
        fixedDraw   <- TradeStrategySearchOperators.randomise[IO](fixed, template)
        invalid     <- TradeStrategySearchOperators.nearby[IO](space, template, -1).attempt
        before      <- IO(random.draws)
        candidate   <- pending
        after       <- IO(random.draws)
      yield (zero, fixedNearby, fixedDraw, invalid, before, candidate, after)
      result.asserting { case (zero, fixedNearby, fixedDraw, invalid, before, candidate, after) =>
        zero mustBe template
        fixedNearby mustBe template
        fixedDraw mustBe template
        invalid.isLeft mustBe true
        before mustBe 0
        after must be > 0
        space.validate(candidate).isRight mustBe true
      }
    }

    "defer random work until evaluation and leave zero-probability operations exact" in {
      given random: CountingRandom = new CountingRandom(19)
      val result                   = for
        crossover <- IO.pure(TradeStrategySearchOperators.crossover[IO](space))
        mutator   <- IO.pure(TradeStrategySearchOperators.mutator[IO](space))
        pending = mutator.mutate(template, 1.0)
        before    <- IO(random.draws)
        unchanged <- mutator.mutate(template, 0.0)
        copied    <- crossover.cross(template, changed(19, C.PriceMovedAgainstEntry(5.0)), 0.0)
        afterZero <- IO(random.draws)
        changed   <- pending
        afterRun  <- IO(random.draws)
      yield (before, unchanged, copied, afterZero, changed, afterRun)
      result.asserting { case (before, unchanged, copied, afterZero, changed, afterRun) =>
        before mustBe 0
        unchanged mustBe template
        copied mustBe template
        afterZero mustBe 0
        changed must not be template
        afterRun must be > 0
      }
    }

    "reject invalid standalone variation probabilities before consuming randomness" in {
      given random: CountingRandom = new CountingRandom(74)
      val crossover                = TradeStrategySearchOperators.crossover[IO](space)
      val mutator                  = TradeStrategySearchOperators.mutator[IO](space)
      val pending                  = List(-0.1, 1.1, Double.NaN, Double.PositiveInfinity).traverse { probability =>
        for
          mutation <- mutator.mutate(template, probability).attempt
          crossing <- crossover.cross(template, template, probability).attempt
        yield List(mutation, crossing)
      }
      random.draws mustBe 0
      pending.asserting { results =>
        results.flatten.foreach(_.left.map(_.getMessage) mustBe Left("Probability must be finite and between zero and one"))
        random.draws mustBe 0
      }
    }

    "keep every numeric coordinate on its grid through mutation and crossover" in {
      given Random = Random(133)
      val result   = for
        crossover <- IO.pure(TradeStrategySearchOperators.crossover[IO](space))
        mutator   <- IO.pure(TradeStrategySearchOperators.mutator[IO](space))
        parents   <- List.fill(80)(template).traverse(TradeStrategySearchOperators.randomise[IO](space, _))
        mutations <- parents.traverse(mutator.mutate(_, 1.0))
        children  <- parents.zip(mutations).traverse((a, b) => crossover.cross(a, b))
      yield mutations ++ children
      result.asserting { candidates =>
        candidates.foreach { candidate =>
          space.validate(candidate) mustBe Right(candidate)
        }
        succeed
      }
    }

    "cross numeric ValueIs levels while retaining the fixed role and operator" in {
      given Random = Random(71)
      val root     = RulePath(RuleList.Open, 0)
      val first    = single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 90))
      val second   = single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 10))
      val domains  = RuleParameterBounds(
        numeric = Map(RuleTarget.At(RuleParameterPath(root, RuleParameter.Level), first) -> DoubleRange(0, 100, 1))
      )
      val local  = TradeStrategySearchSpace.forStrategy(first, indicator, domains).toOption.get
      val result = for
        crossover <- IO.pure(TradeStrategySearchOperators.crossover[IO](local))
        children  <- List.fill(100)(()).traverse(_ => crossover.cross(first, second))
      yield children
      result.asserting { children =>
        children.distinct.size must be > 2
        children.foreach { child =>
          local.validate(child) mustBe Right(child)
          child.openRules.head.conditions match
            case C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, level) =>
              level must be >= 10.0
              level must be <= 90.0
            case other => fail(s"Unexpected changed role or operator: $other")
        }
        local.parameterCount mustBe 1
      }
    }

    "draw mutation decisions only for the active level when role and operator are fixed" in {
      val root    = RulePath(RuleList.Open, 0)
      val base    = single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50))
      val domains = RuleParameterBounds(
        numeric = Map(RuleTarget.At(RuleParameterPath(root, RuleParameter.Level), base) -> DoubleRange(0, 100, 1))
      )
      val local         = TradeStrategySearchSpace.forStrategy(base, indicator, domains).toOption.get
      var decisions     = 0
      var gaussianDraws = 0
      given Random      = new Random(1):
        override def nextDouble(): Double   = { decisions += 1; if (decisions == 1) 0.9 else 0.1 }
        override def nextGaussian(): Double = { gaussianDraws += 1; 1.0 }
      val result = for
        mutator <- IO.pure(TradeStrategySearchOperators.mutator[IO](local))
        skipped <- mutator.mutate(base, 0.5)
        mutated <- mutator.mutate(base, 0.5)
      yield (skipped, mutated)
      result.asserting { case (skipped, mutated) =>
        local.parameterCount mustBe 1
        skipped mustBe base
        mutated mustBe single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 60))
        decisions mustBe 2
        gaussianDraws mustBe 1
      }
    }

    "reject different ValueIs role inputs and exclude incompatible extra seeds before consuming randomness" in {
      given random: CountingRandom = new CountingRandom(54)
      val root                     = RulePath(RuleList.Open, 0)
      val base                     = single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50))
      val otherRole                = single(C.ValueIs(ValueRole.Price, Rule.Operator.GreaterThan, 50))
      val domains                  = RuleParameterBounds(
        numeric = Map(RuleTarget.At(RuleParameterPath(root, RuleParameter.Level), base) -> DoubleRange(0, 100, 1))
      )
      val local     = TradeStrategySearchSpace.forStrategy(base, indicator, domains).toOption.get
      val crossover = TradeStrategySearchOperators.crossover[IO](local)
      val mutator   = TradeStrategySearchOperators.mutator[IO](local)
      val result    = for
        populationBuilder <- initialiser(base, domains, List(otherRole))
        crossed           <- crossover.cross(base, otherRole).attempt
        skippedCross      <- crossover.cross(base, otherRole, 0).attempt
        mutated           <- mutator.mutate(otherRole, 0).attempt
        sampled           <- TradeStrategySearchOperators.randomise[IO](local, otherRole).attempt
        initialSeed       <- populationBuilder.initialisePopulation(otherRole, 10, shuffle = true).attempt
        extraSeed         <- populationBuilder.initialisePopulation(base, 1, shuffle = true)
      yield (List(crossed, skippedCross, mutated, sampled, initialSeed), extraSeed)
      result.asserting { (errors, extraSeed) =>
        errors.foreach(error => error.swap.toOption.get.getMessage must include(root.toString))
        extraSeed mustBe Vector(base)
        random.draws mustBe 0
      }
    }

    "keep generated and restored durations in their coarsest exact units" in {
      given Random = new Random(13):
        override def nextInt(bound: Int): Int = 2
        override def nextGaussian(): Double   = 1.0
        override def nextDouble(): Double     = 0.5
      val base  = single(C.PositionOpenFor(3.hours.toNanos.nanos))
      val local = TradeStrategySearchSpace
        .forStrategy(
          base,
          indicator,
          RuleParameterBounds(
            durations = Map(RuleTarget.HoldingTime -> DurationRange(1.hour, 11.hours, 1.hour))
          )
        )
        .toOption
        .get
      val fixed = TradeStrategySearchSpace
        .forStrategy(
          base,
          indicator,
          RuleParameterBounds(
            durations = Map(RuleTarget.HoldingTime -> DurationRange(3.hours, 3.hours, 1.hour))
          )
        )
        .toOption
        .get
      val result = for
        crossover <- IO.pure(TradeStrategySearchOperators.crossover[IO](local))
        mutator   <- IO.pure(TradeStrategySearchOperators.mutator[IO](local))
        sample    <- TradeStrategySearchOperators.randomise[IO](local, base)
        mutation  <- mutator.mutate(base, 1.0)
        child     <- crossover.cross(base, single(C.PositionOpenFor(5.hours)))
      yield List(sample, mutation, child)
      result.asserting { candidates =>
        candidates.map(_.toString) mustBe List(3, 4, 4).map(n => single(C.PositionOpenFor(n.hours)).toString)
        fixed.projectSeed(single(C.PositionOpenFor(5.hours))).toOption.get.toString mustBe single(C.PositionOpenFor(3.hours)).toString
      }
    }

    "produce the same candidates when bounds maps have a different insertion order" in {
      def run(reverse: Boolean): IO[Vector[TradeStrategy]] =
        given Random = Random(912)
        val ordered  = linkedBounds.copy(
          durations = scala.collection.immutable.ListMap.from(
            if (reverse) linkedBounds.durations.toList.reverse else linkedBounds.durations.toList
          )
        )
        initialiser(mirrored(), ordered).flatMap(_.initialisePopulation(mirrored(), 50, shuffle = true))
      (for
        first  <- run(false)
        second <- run(true)
      yield (first, second)).asserting { case (first, second) => first mustBe second }
    }

    "use exact nanosecond grid indices above Double's integer precision" in {
      given Random = Random(95)
      val offset   = (1L << 53) + 1
      val root     = RulePath(RuleList.Open, 0)
      val base     = single(C.PositionOpenFor((offset + 3).nanos))
      val domains  = RuleParameterBounds(durations =
        Map(
          RuleTarget.At(RuleParameterPath(root, RuleParameter.Duration), base) ->
            DurationRange(offset.nanos, (offset + 10).nanos, 1.nano)
        )
      )
      val local  = TradeStrategySearchSpace.forStrategy(base, indicator, domains).toOption.get
      val result = for
        crossover <- IO.pure(TradeStrategySearchOperators.crossover[IO](local))
        mutator   <- IO.pure(TradeStrategySearchOperators.mutator[IO](local))
        samples   <- List.fill(100)(base).traverse(TradeStrategySearchOperators.randomise[IO](local, _))
        mutated   <- samples.traverse(mutator.mutate(_, 1.0))
        crossed   <- samples.zip(mutated).traverse((a, b) => crossover.cross(a, b))
      yield samples ++ mutated ++ crossed
      result.asserting { candidates =>
        candidates.foreach(candidate => local.validate(candidate).isRight mustBe true)
        val values = candidates.flatMap(_.openRules.map(_.conditions)).collect { case C.PositionOpenFor(d) => d.toNanos }
        values.toSet must contain(offset)
        values.toSet must contain(offset + 1)
        values.toSet must contain(offset + 10)
      }
    }

    "accept representable Double values produced by decimal grid arithmetic" in {
      given Random = Random(881)
      val root     = RulePath(RuleList.Open, 0)
      val base     = single(C.PriceMovedAgainstEntry(1.0))
      val domains  = RuleParameterBounds(numeric =
        Map(
          RuleTarget.At(RuleParameterPath(root, RuleParameter.AtrMultiplier), base) ->
            DoubleRange(1.0, 1.000000000000003, 3e-16)
        )
      )
      val local  = TradeStrategySearchSpace.forStrategy(base, indicator, domains).toOption.get
      val result = for
        crossover <- IO.pure(TradeStrategySearchOperators.crossover[IO](local))
        mutator   <- IO.pure(TradeStrategySearchOperators.mutator[IO](local))
        samples   <- List.fill(100)(base).traverse(TradeStrategySearchOperators.randomise[IO](local, _))
        mutated   <- samples.traverse(mutator.mutate(_, 1.0))
        crossed   <- samples.zip(mutated).traverse((a, b) => crossover.cross(a, b))
      yield samples ++ mutated ++ crossed
      result.asserting { candidates =>
        candidates.foreach(candidate => local.validate(candidate).isRight mustBe true)
        candidates.distinct.size must be > 1
        // Grid index 4 rounds to this Double; nearest-index arithmetic on its shortest decimal text incorrectly picks index 3.
        local.validate(single(C.PriceMovedAgainstEntry(1.000000000000001))).isRight mustBe true
      }
    }

    "avoid random draws for singleton and parameter-free searches" in {
      given random: CountingRandom = new CountingRandom(34)
      val root                     = RulePath(RuleList.Open, 0)
      val base                     = single(C.ValueIs(ValueRole.Momentum, Rule.Operator.EqualTo, 50))
      val domains                  = RuleParameterBounds(
        numeric = Map(RuleTarget.At(RuleParameterPath(root, RuleParameter.Level), base) -> DoubleRange(50, 50, 1))
      )
      val local  = TradeStrategySearchSpace.forStrategy(base, indicator, domains).toOption.get
      val result = for
        crossover  <- IO.pure(TradeStrategySearchOperators.crossover[IO](local))
        mutator    <- IO.pure(TradeStrategySearchOperators.mutator[IO](local))
        population <- Vector.fill(20)(base).traverse(TradeStrategySearchOperators.randomise[IO](local, _))
        mutated    <- mutator.mutate(base, 1.0)
        child      <- crossover.cross(base, base, 1.0)
      yield (population, mutated, child, random.draws)
      result.asserting { case (population, mutated, child, draws) =>
        local.parameterCount mustBe 0
        population mustBe Vector.fill(20)(base)
        mutated mustBe base
        child mustBe base
        draws mustBe 0
      }
    }

    "replay the same population and mutations from the same seed" in {
      def run(seed: Long): IO[Vector[TradeStrategy]] =
        given Random = Random(seed)
        for
          populationBuilder <- initialiser(template, bounds)
          mutator           <- IO.pure(TradeStrategySearchOperators.mutator[IO](space))
          population        <- populationBuilder.initialisePopulation(template, 30, shuffle = true)
          mutated           <- population.traverse(mutator.mutate(_, 0.3))
        yield mutated
      (for first <- run(246); repeated <- run(246); different <- run(247) yield (first, repeated, different)).asserting {
        case (first, repeated, different) =>
          first mustBe repeated
          first must not be different
      }
    }
  }
}
