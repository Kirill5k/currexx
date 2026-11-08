package currexx.core.trade

import currexx.core.market.MomentumZone
import currexx.core.trade.Rule.{Condition, Operator}
import currexx.domain.market.TradeOrder
import currexx.domain.signal.{Direction, ValueRole, VolatilityRegime}
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.*

class TradeStrategyConditionSpec extends AnyWordSpec with Matchers {

  "TradeStrategy.Condition codecs" should {
    "encode and decode all 23 condition variants to and from json" in {
      val conditions: List[(Condition, String)] = List(
        Condition.AllOf(List(Condition.NoPosition)) ->
          """{"conditions":[{"kind":"no-position"}],"kind":"all-of"}""",
        Condition.AnyOf(List(Condition.NoPosition)) ->
          """{"conditions":[{"kind":"no-position"}],"kind":"any-of"}""",
        Condition.Not(Condition.NoPosition) ->
          """{"condition":{"kind":"no-position"},"kind":"not"}""",
        Condition.TrendChangedTo(Direction.Upward) ->
          """{"direction":"upward","kind":"trend-changed-to"}""",
        Condition.TrendIs(Direction.Downward) ->
          """{"direction":"downward","kind":"trend-is"}""",
        Condition.TrendActiveFor(60.seconds) ->
          """{"duration":"1 minute","kind":"trend-active-for"}""",
        Condition.CrossoverOccurred(Direction.Upward) ->
          """{"direction":"upward","kind":"crossover-occurred"}""",
        Condition.MomentumEntered(MomentumZone.Overbought) ->
          """{"zone":"overbought","kind":"momentum-entered"}""",
        Condition.MomentumIsIn(MomentumZone.Oversold) ->
          """{"zone":"oversold","kind":"momentum-is-in"}""",
        Condition.MomentumIs(Direction.Still) ->
          """{"direction":"still","kind":"momentum-is"}""",
        Condition.PreviousVolatilityIs(VolatilityRegime.High) ->
          """{"regime":"high","kind":"previous-volatility-is"}""",
        Condition.VolatilityIs(VolatilityRegime.Low) ->
          """{"regime":"low","kind":"volatility-is"}""",
        Condition.VelocityIs(Direction.Upward) ->
          """{"direction":"upward","kind":"velocity-is"}""",
        Condition.VelocityCrossedLevel(1.5, Direction.Downward) ->
          """{"level":1.5,"direction":"downward","kind":"velocity-crossed-level"}""",
        Condition.VelocityIsBelow(0.0) ->
          """{"threshold":0.0,"kind":"velocity-is-below"}""",
        Condition.PositionIs(TradeOrder.Position.Buy) ->
          """{"position":"buy","kind":"position-is"}""",
        Condition.PositionOpenFor(120.seconds) ->
          """{"duration":"2 minutes","kind":"position-open-for"}""",
        Condition.NoPosition ->
          """{"kind":"no-position"}""",
        Condition.UpperBandCrossed(Direction.Upward) ->
          """{"direction":"upward","kind":"upper-band-crossed"}""",
        Condition.LowerBandCrossed(Direction.Downward) ->
          """{"direction":"downward","kind":"lower-band-crossed"}""",
        Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward) ->
          """{"lineRole":"channel-middle-band","direction":"upward","kind":"price-crossed-line"}""",
        Condition.ValueIs(ValueRole.Momentum, Operator.GreaterThan, 50.0) ->
          """{"role":"momentum","operator":"greater-than","level":50.0,"kind":"value-is"}""",
        Condition.PriceMovedAgainstEntry(1.5) ->
          """{"nAtr":1.5,"kind":"price-moved-against-entry"}"""
      )

      conditions.foreach { case (cond, json) =>
        cond.asJson.noSpaces mustBe json
        decode[Condition](json) mustBe Right(cond)
      }
    }

    "fail to decode unknown condition kind" in {
      decode[Condition]("""{"kind":"unknown-condition"}""").isLeft mustBe true
    }
  }
}
