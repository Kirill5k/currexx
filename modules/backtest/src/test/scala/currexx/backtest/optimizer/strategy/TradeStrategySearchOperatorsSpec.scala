package currexx.backtest.optimizer.strategy

import cats.effect.IO
import cats.syntax.traverse.*
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

  "Trade strategy operators" should {
    "preserve grids, fixed fields and linked groups through every variation operation" in {
      given Random = Random(105)
      val linked   = TradeStrategySearchSpace.forStrategy(mirrored(), indicator, linkedBounds).toOption.get
      List(space, linked)
        .traverse { local =>
          for
            sampled <- List.fill(32)(local.template).traverse(TradeStrategySearchOperators.randomise[IO](local, _))
            mutated <- sampled.traverse(TradeStrategySearchOperators.mutateChecked[IO](local, _, 1.0))
            crossed <- sampled.zip(mutated).traverse((a, b) => TradeStrategySearchOperators.cross[IO](local, a, b))
            nearby  <- sampled.traverse(TradeStrategySearchOperators.nearby[IO](local, _, 3.0))
          yield (local, List(sampled, mutated, crossed, nearby))
        }
        .asserting { results =>
          results.foreach { (local, operations) =>
            operations.foreach { candidates =>
              candidates.distinct.size must be > 1
              candidates.foreach(candidate => local.validate(candidate) mustBe Right(candidate))
            }
            val leaves = operations.flatten.map(RuleTree.leaves(_).map(_._2))
            if (local == space) {
              val numericIndices = Set(2, 10, 11, 13, 18, 19)
              conditions.indices.foreach { index =>
                if (numericIndices.contains(index)) leaves.map(_(index)).distinct.size must be > 1
                else leaves.map(_(index)).distinct mustBe List(conditions(index))
              }
              val levels = leaves.map(_(18)).collect { case value: C.ValueIs => value }
              levels.map(_.role).toSet mustBe Set(ValueRole.Momentum)
              levels.map(_.operator).toSet mustBe Set(Rule.Operator.GreaterThan)
            } else {
              local.parameterCount mustBe 3
              leaves.foreach { values =>
                val ages  = values.collect { case C.TrendActiveFor(value) => value }
                val times = values.collect { case C.PositionOpenFor(value) => value }
                val stops = values.collect { case C.PriceMovedAgainstEntry(value) => value }
                List(ages.size, times.size, stops.size) mustBe List(2, 2, 2)
                List(ages.distinct.size, times.distinct.size, stops.distinct.size) mustBe List(1, 1, 1)
              }
            }
          }
          succeed
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
        sampled <- TradeStrategySearchOperators.randomise[IO](local, base)
        mutated <- TradeStrategySearchOperators.mutateChecked[IO](local, base, 1.0)
        child   <- TradeStrategySearchOperators.cross[IO](local, base, mirrored(6.hours, 24.hours, 6.0))
      yield List(sampled, mutated, child)
      result.asserting { candidates =>
        samples mustBe 3
        gaussians mustBe 3
        decisions mustBe 6
        candidates.foreach(candidate => local.validate(candidate).isRight mustBe true)
        succeed
      }
    }

    "defer every random operation and avoid draws for zero changes or fixed coordinates" in {
      given random: CountingRandom = new CountingRandom(43)
      val pending                  = List(
        TradeStrategySearchOperators.randomise[IO](space, template),
        TradeStrategySearchOperators.mutateChecked[IO](space, template, 1.0),
        TradeStrategySearchOperators.nearby[IO](space, template, 1.0),
        TradeStrategySearchOperators.cross[IO](space, template, changed(19, C.PriceMovedAgainstEntry(5.0)))
      )
      val fixedBounds = List(RuleParameterBounds(), RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> DoubleRange(4, 4, 0.25))))
      val fixed       = fixedBounds.map(TradeStrategySearchSpace.forStrategy(template, indicator, _).toOption.get)
      val noChanges   = List(
        TradeStrategySearchOperators.nearby[IO](space, template, 0),
        TradeStrategySearchOperators.mutateChecked[IO](space, template, 0)
      ) ++ fixed.flatMap { local =>
        List(
          TradeStrategySearchOperators.randomise[IO](local, template),
          TradeStrategySearchOperators.nearby[IO](local, template, 6),
          TradeStrategySearchOperators.mutateChecked[IO](local, template, 1),
          TradeStrategySearchOperators.cross[IO](local, template, template)
        )
      }
      random.draws mustBe 0
      val result = for
        unchanged  <- noChanges.sequence
        invalid    <- TradeStrategySearchOperators.nearby[IO](space, template, -1).attempt
        before     <- IO(random.draws)
        candidates <- pending.traverse { operation =>
          for
            start     <- IO(random.draws)
            candidate <- operation
            end       <- IO(random.draws)
          yield (candidate, end - start)
        }
      yield (unchanged, invalid, before, candidates)
      result.asserting { (unchanged, invalid, before, candidates) =>
        unchanged mustBe List.fill(noChanges.size)(template)
        invalid.isLeft mustBe true
        before mustBe 0
        candidates.foreach { (candidate, draws) =>
          space.validate(candidate) mustBe Right(candidate)
          draws must be > 0
        }
        succeed
      }
    }

    "mutate and interpolate ValueIs levels without drawing for its fixed role or operator" in {
      val root    = RulePath(RuleList.Open, 0)
      val base    = single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 50))
      val domains = RuleParameterBounds(
        numeric = Map(RuleTarget.At(RuleParameterPath(root, RuleParameter.Level), base) -> DoubleRange(0, 100, 1))
      )
      val local         = TradeStrategySearchSpace.forStrategy(base, indicator, domains).toOption.get
      var decisions     = 0
      var gaussianDraws = 0
      given Random      = new Random(1):
        override def nextDouble(): Double   = { decisions += 1; if (decisions == 1) 0.9 else if (decisions == 2) 0.1 else 0.5 }
        override def nextGaussian(): Double = { gaussianDraws += 1; 1.0 }
      val result = for
        skipped <- TradeStrategySearchOperators.mutateChecked[IO](local, base, 0.5)
        mutated <- TradeStrategySearchOperators.mutateChecked[IO](local, base, 0.5)
        child   <- TradeStrategySearchOperators.cross[IO](
          local,
          single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 90)),
          single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 10))
        )
      yield (skipped, mutated, child)
      result.asserting { (skipped, mutated, child) =>
        local.parameterCount mustBe 1
        skipped mustBe base
        mutated mustBe single(C.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 60))
        child mustBe base
        decisions mustBe 3
        gaussianDraws mustBe 1
      }
    }

    "reject incompatible rule inputs before consuming randomness" in {
      given random: CountingRandom = new CountingRandom(54)
      val otherRole                = changed(18, C.ValueIs(ValueRole.Price, Rule.Operator.GreaterThan, 50))
      List(
        TradeStrategySearchOperators.cross[IO](space, template, otherRole),
        TradeStrategySearchOperators.mutateChecked[IO](space, otherRole, 0),
        TradeStrategySearchOperators.randomise[IO](space, otherRole),
        TradeStrategySearchOperators.nearby[IO](space, otherRole, 0)
      ).traverse(_.attempt).asserting { errors =>
        errors.foreach(error => error.swap.toOption.get.getMessage must include(path(18).toString))
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
          RuleParameterBounds(durations = Map(RuleTarget.HoldingTime -> DurationRange(1.hour, 11.hours, 1.hour)))
        )
        .toOption
        .get
      val fixed = TradeStrategySearchSpace
        .forStrategy(
          base,
          indicator,
          RuleParameterBounds(durations = Map(RuleTarget.HoldingTime -> DurationRange(3.hours, 3.hours, 1.hour)))
        )
        .toOption
        .get
      val result = for
        sample   <- TradeStrategySearchOperators.randomise[IO](local, base)
        mutation <- TradeStrategySearchOperators.mutateChecked[IO](local, base, 1.0)
        child    <- TradeStrategySearchOperators.cross[IO](local, base, single(C.PositionOpenFor(5.hours)))
      yield List(sample, mutation, child)
      result.asserting { candidates =>
        candidates.map(_.toString) mustBe List(3, 4, 4).map(n => single(C.PositionOpenFor(n.hours)).toString)
        fixed.projectSeed(single(C.PositionOpenFor(5.hours))).toOption.get.toString mustBe single(C.PositionOpenFor(3.hours)).toString
      }
    }

    "retain precise decimal and nanosecond values when rebuilding rules" in {
      given Random = new Random(1):
        override def nextInt(bound: Int): Int = 4
        override def nextGaussian(): Double   = 0.0
        override def nextDouble(): Double     = 0.5
      val offset   = (1L << 53) + 1
      val fixtures = List(
        (
          single(C.PriceMovedAgainstEntry(1.0)),
          RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> DoubleRange(1.0, 1.000000000000003, 3e-16))),
          single(C.PriceMovedAgainstEntry(1.000000000000001))
        ),
        (
          single(C.PositionOpenFor((offset + 3).nanos)),
          RuleParameterBounds(durations = Map(RuleTarget.HoldingTime -> DurationRange(offset.nanos, (offset + 10).nanos, 1.nano))),
          single(C.PositionOpenFor((offset + 4).nanos))
        )
      )
      fixtures
        .traverse { (base, bounds, expected) =>
          val local = TradeStrategySearchSpace.forStrategy(base, indicator, bounds).toOption.get
          for
            sampled <- TradeStrategySearchOperators.randomise[IO](local, base)
            mutated <- TradeStrategySearchOperators.mutateChecked[IO](local, sampled, 1)
            crossed <- TradeStrategySearchOperators.cross[IO](local, base, sampled)
          yield (local, expected, sampled, mutated, crossed)
        }
        .asserting { outcomes =>
          outcomes.foreach { (local, expected, sampled, mutated, crossed) =>
            sampled mustBe expected
            mutated mustBe sampled
            local.validate(crossed) mustBe Right(crossed)
          }
          succeed
        }
    }

    "replay variation from its seed independently of bounds map insertion order" in {
      def run(seed: Long, reverse: Boolean): IO[List[TradeStrategy]] =
        given Random = Random(seed)
        val ordered  = linkedBounds.copy(
          durations = scala.collection.immutable.ListMap.from(
            if (reverse) linkedBounds.durations.toList.reverse else linkedBounds.durations.toList
          )
        )
        val local = TradeStrategySearchSpace.forStrategy(mirrored(), indicator, ordered).toOption.get
        for
          samples <- List.fill(16)(mirrored()).traverse(TradeStrategySearchOperators.randomise[IO](local, _))
          mutated <- samples.traverse(TradeStrategySearchOperators.mutateChecked[IO](local, _, 0.3))
        yield samples ++ mutated
      (for
        first     <- run(912, false)
        repeated  <- run(912, false)
        reordered <- run(912, true)
        different <- run(913, false)
      yield (first, repeated, reordered, different)).asserting { (first, repeated, reordered, different) =>
        first mustBe repeated
        first mustBe reordered
        first must not be different
      }
    }
  }
}
