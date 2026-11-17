package currexx.backtest.optimizer.indicator

import cats.effect.IO
import cats.syntax.traverse.*
import currexx.backtest.optimizer.indicator.bounds.IndicatorBounds
import currexx.backtest.optimizer.strategy.{RuleParameterBounds, RuleTarget, TradeStrategySearchOperators, TradeStrategySearchSpace}
import currexx.core.trade.{Rule, StrategyCheck, TradeAction, TradeStrategy}
import currexx.domain.signal.{Indicator, IndicatorId, ValueRole, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class NamedIndicatorSpec extends IOWordSpec {
  private val fast                                          = IndicatorId("fast-flow")
  private val slow                                          = IndicatorId("slow-flow")
  private def flow(id: IndicatorId, length: Int): Indicator =
    Indicator.Named(id, Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.CMF(length)))
  private val template = Indicator.compositeAnyOf(flow(fast, 20), flow(slow, 40))

  "Named indicators" should {
    "preserve names through initialisation, mutation, crossover and bounds repair" in {
      given Random = Random(42)
      (for
        initialiser  <- IndicatorInitialiser.make[IO]
        mutator      <- IndicatorMutator.make[IO]
        crossover    <- IndicatorCrossover.make[IO]
        sampled      <- initialiser.initialisePopulation(template, 32, true)
        mutated      <- sampled.toList.traverse(mutator.mutate(_, 1.0))
        crossed      <- sampled.toList.zip(mutated).traverse((a, b) => crossover.cross(a, b))
        incompatible <- crossover.cross(flow(fast, 20), flow(slow, 40)).attempt
      yield (sampled.toList ++ mutated ++ crossed, incompatible)).asserting { (candidates, incompatible) =>
        candidates.distinct.size must be > 1
        candidates.foreach { candidate =>
          candidate.leaves.flatMap(_.resolvedId) mustBe List(fast, slow)
          IndicatorBounds.repair(candidate).leaves.flatMap(_.resolvedId) mustBe List(fast, slow)
          StrategyCheck.validate(List(candidate), None).isRight mustBe true
        }
        incompatible.isLeft mustBe true
        IndicatorBounds.repair(flow(fast, 10000)).resolvedId mustBe Some(fast)
      }
    }

    "preserve rule references through every variation operation" in {
      given Random   = Random(17)
      val price      = IndicatorId("close-price")
      val atr        = IndicatorId("stop-atr")
      val indicators = Indicator.compositeAnyOf(
        template,
        Indicator.Named(price, Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1))),
        Indicator.Named(atr, Indicator.ValueTracking(ValueRole.Volatility, ValueSource.Close, VT.ATR(14)))
      )
      val base = TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.PriceMovedAgainstEntry(2, Some(price), Some(atr)))))
      val bounds = RuleParameterBounds(numeric = Map(RuleTarget.AtrStop -> RuleParameterBounds.DoubleRange(1, 5, 0.25)))
      val space  = TradeStrategySearchSpace.forStrategy(base, indicators, bounds).toOption.get
      (for
        sampled <- TradeStrategySearchOperators.randomise[IO](space, base)
        mutated <- TradeStrategySearchOperators.mutateChecked[IO](space, sampled, 1)
        crossed <- TradeStrategySearchOperators.cross[IO](space, base, mutated)
        nearby  <- TradeStrategySearchOperators.nearby[IO](space, base, 2)
      yield List(sampled, mutated, crossed, nearby)).asserting { candidates =>
        candidates.foreach { candidate =>
          space.validate(candidate).isRight mustBe true
          candidate.closeRules.flatMap(r => StrategyCheck.inputs(r.conditions).map(_.id)) mustBe List(price, atr)
        }
        succeed
      }
    }
  }
}
