package currexx.core.trade

import currexx.core.market.LegacyMarketProfile.*

import currexx.core.fixtures.Markets.*
import currexx.core.market.{MarketProfile, MarketState, MomentumZone, PositionState, Reading}
import currexx.core.trade.Rule.Condition
import currexx.domain.market.TradeOrder
import currexx.domain.signal.{Boundary, Direction, IndicatorId, ValueRole, VolatilityRegime}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.*

class TradeStrategySpec extends AnyWordSpec with Matchers {

  val tradeAction: TradeAction         = TradeAction.OpenLong
  val anotherTradeAction: TradeAction  = TradeAction.OpenShort
  val closePositionAction: TradeAction = TradeAction.ClosePosition

  val previousProfile: MarketProfile = MarketProfile.fromLegacy(MarketProfile.toLegacy(profile).copy(lastMomentumValue = Some(10.0)))
  val trendChangedState: MarketState = state.copy(
    profile = MarketProfile.fromLegacy(
      MarketProfile
        .toLegacy(profile)
        .copy(
          trend = Some(TrendState(Direction.Downward, ts)),
          crossover = Some(CrossoverState(Direction.Downward, ts)),
          momentum = Some(MomentumState(MomentumZone.Oversold, ts)),
          volatility = Some(VolatilityState(VolatilityRegime.High, ts)),
          lastMomentumValue = Some(20.0)
        )
    )
  )
  val trendActiveForState: MarketState = trendChangedState.copy(
    profile = MarketProfile.fromLegacy(
      MarketProfile
        .toLegacy(trendChangedState.profile)
        .copy(
          trend = trendChangedState.profile.trend().map(t => TrendState(t.direction, ts.minusSeconds(100)))
        )
    )
  )
  val momentumIsState: MarketState =
    trendChangedState.copy(profile =
      MarketProfile.fromLegacy(MarketProfile.toLegacy(trendChangedState.profile).copy(lastMomentumValue = Some(20.0)))
    )
  val positionOpenForState: MarketState = trendChangedState.copy(
    currentPosition = Some(positionState.copy(openedAt = ts.minusSeconds(70)))
  )
  val noPositionState: MarketState    = trendChangedState.copy(currentPosition = None)
  val momentumStillState: MarketState =
    trendChangedState.copy(profile =
      MarketProfile.fromLegacy(MarketProfile.toLegacy(trendChangedState.profile).copy(lastMomentumValue = Some(10.0)))
    )
  val momentumEnteredState: MarketState = trendChangedState.copy(
    profile = MarketProfile.fromLegacy(
      MarketProfile.toLegacy(trendChangedState.profile).copy(momentum = Some(MomentumState(MomentumZone.Overbought, ts)))
    )
  )
  val crossoverOccurredState: MarketState = trendChangedState.copy(
    profile = MarketProfile.fromLegacy(
      MarketProfile.toLegacy(trendChangedState.profile).copy(crossover = Some(CrossoverState(Direction.Downward, ts)))
    )
  )

  val upperBandCrossedState: MarketState = trendChangedState.copy(
    profile = MarketProfile.fromLegacy(
      MarketProfile
        .toLegacy(trendChangedState.profile)
        .copy(lastBandCrossing = Some(BandCrossingState(Boundary.Upper, Direction.Upward, ts)))
    )
  )
  val lowerBandCrossedState: MarketState = trendChangedState.copy(
    profile = MarketProfile.fromLegacy(
      MarketProfile
        .toLegacy(trendChangedState.profile)
        .copy(lastBandCrossing = Some(BandCrossingState(Boundary.Lower, Direction.Downward, ts)))
    )
  )
  val priceCrossedLineState: MarketState = trendChangedState.copy(
    profile = MarketProfile.fromLegacy(
      MarketProfile
        .toLegacy(trendChangedState.profile)
        .copy(lastPriceLineCrossing = Some(PriceLineCrossingState(ValueRole.ChannelMiddleBand, Direction.Upward, ts)))
    )
  )

  "Rule readiness" should {
    def firesReady(condition: Condition, current: MarketState): Boolean =
      Rule
        .findTriggeredAction(List(Rule(TradeAction.ClosePosition, condition)), current, current.previousProfile.getOrElse(MarketProfile()))
        .nonEmpty

    "keep a negated zone unknown without disabling an independent price stop" in {
      val price   = IndicatorId.forValue(ValueRole.Price)
      val atr     = IndicatorId.forValue(ValueRole.Volatility)
      val current = state.copy(
        invalidated = Set(IndicatorId.MomentumZone),
        currentPosition = Some(PositionState(TradeOrder.Position.Buy, ts, Some(BigDecimal("1.2")))),
        profile = MarketProfile().updated(price, Reading.Value(BigDecimal("1.1"))).updated(atr, Reading.Value(BigDecimal("0.01")))
      )
      val negated = Rule.Condition.Not(Rule.Condition.MomentumIsIn(MomentumZone.Overbought))
      firesReady(negated, current) mustBe false
      firesReady(Rule.Condition.anyOf(negated, Rule.Condition.PriceMovedAgainstEntry(2)), current) mustBe true
      firesReady(
        Rule.Condition.Not(Rule.Condition.PriceMovedAgainstEntry(2)),
        current.copy(profile = MarketProfile(), invalidated = Set(price, atr))
      ) mustBe false
      firesReady(
        Rule.Condition.Not(Rule.Condition.PriceMovedAgainstEntry(2)),
        current.copy(currentPosition = None, profile = MarketProfile(), invalidated = Set(price, atr))
      ) mustBe true
    }

    "withhold history conditions until comparable history exists" in {
      val momentum = IndicatorId.forValue(ValueRole.Momentum)
      val current  = state.copy(
        profile = MarketProfile().updated(momentum, Reading.Value(BigDecimal("0.1"))),
        previousProfile = Some(MarketProfile()),
        invalidated = Set(momentum)
      )
      firesReady(Condition.MomentumIs(Direction.Still), current) mustBe false
      firesReady(
        Condition.MomentumIs(Direction.Still),
        current.copy(previousProfile = Some(current.profile), invalidated = Set.empty)
      ) mustBe true
    }
  }

  "A Rule.findTriggeredAction" should {
    "return None when no rules are provided" in {
      Rule.findTriggeredAction(Nil, trendChangedState, previousProfile) mustBe None
    }

    "return None when no rules are triggered" in {
      val rules = List(Rule(tradeAction, Condition.TrendIs(Direction.Upward)))
      Rule.findTriggeredAction(rules, trendChangedState, previousProfile) mustBe None
    }

    "return the action of the first triggered rule" in {
      val rules = List(
        Rule(anotherTradeAction, Condition.TrendIs(Direction.Upward)),
        Rule(tradeAction, Condition.PositionIs(TradeOrder.Position.Buy))
      )
      Rule.findTriggeredAction(rules, trendChangedState, previousProfile) mustBe Some(tradeAction)
    }

    "evaluate TrendChangedTo condition" in {
      val rule = Rule(tradeAction, Condition.TrendChangedTo(Direction.Downward))
      Rule.findTriggeredAction(List(rule), trendChangedState, previousProfile) mustBe Some(tradeAction)
    }

    "evaluate TrendIs condition" in {
      val rule = Rule(tradeAction, Condition.TrendIs(Direction.Downward))
      Rule.findTriggeredAction(List(rule), trendChangedState, previousProfile) mustBe Some(tradeAction)
    }

    "evaluate TrendActiveFor condition" in {
      val rule = Rule(closePositionAction, Condition.TrendActiveFor(90.seconds))
      Rule.findTriggeredAction(List(rule), trendActiveForState, previousProfile) mustBe Some(closePositionAction)
    }

    "evaluate CrossoverOccurred condition" in {
      val rule = Rule(tradeAction, Condition.CrossoverOccurred(Direction.Downward))
      Rule.findTriggeredAction(List(rule), crossoverOccurredState, previousProfile) mustBe Some(tradeAction)
    }

    "evaluate MomentumEntered condition" in {
      val rule = Rule(tradeAction, Condition.MomentumEntered(MomentumZone.Overbought))
      Rule.findTriggeredAction(List(rule), momentumEnteredState, previousProfile) mustBe Some(tradeAction)
    }

    "evaluate MomentumIsIn condition" in {
      val rule = Rule(tradeAction, Condition.MomentumIsIn(MomentumZone.Oversold))
      Rule.findTriggeredAction(List(rule), trendChangedState, previousProfile) mustBe Some(tradeAction)
    }

    "evaluate MomentumIs condition" in {
      val upwardRule = Rule(tradeAction, Condition.MomentumIs(Direction.Upward))
      Rule.findTriggeredAction(List(upwardRule), momentumIsState, previousProfile) mustBe Some(tradeAction)

      val stillRule = Rule(anotherTradeAction, Condition.MomentumIs(Direction.Still))
      Rule.findTriggeredAction(List(stillRule), momentumStillState, previousProfile) mustBe Some(anotherTradeAction)
    }

    "evaluate VolatilityIs condition" in {
      val rule = Rule(tradeAction, Condition.VolatilityIs(VolatilityRegime.High))
      Rule.findTriggeredAction(List(rule), trendChangedState, previousProfile) mustBe Some(tradeAction)
    }

    "evaluate PositionIsOpen condition" in {
      val rule = Rule(tradeAction, Condition.PositionIs(TradeOrder.Position.Buy))
      Rule.findTriggeredAction(List(rule), trendChangedState, previousProfile) mustBe Some(tradeAction)
    }

    "evaluate NoPosition condition" in {
      val rule = Rule(tradeAction, Condition.NoPosition)
      Rule.findTriggeredAction(List(rule), noPositionState, previousProfile) mustBe Some(tradeAction)
    }

    "evaluate PositionOpenFor condition" in {
      val rule = Rule(closePositionAction, Condition.PositionOpenFor(60.seconds))
      Rule.findTriggeredAction(List(rule), positionOpenForState, previousProfile) mustBe Some(closePositionAction)
    }

    "evaluate AllOf condition" in {
      val rule = Rule(
        closePositionAction,
        Condition.AllOf(List(Condition.PositionIs(TradeOrder.Position.Buy), Condition.TrendIs(Direction.Downward)))
      )
      Rule.findTriggeredAction(List(rule), trendChangedState, previousProfile) mustBe Some(closePositionAction)
    }

    "evaluate AnyOf condition" in {
      val rule = Rule(closePositionAction, Condition.AnyOf(List(Condition.NoPosition, Condition.TrendIs(Direction.Downward))))
      Rule.findTriggeredAction(List(rule), trendChangedState, previousProfile) mustBe Some(closePositionAction)
    }

    "evaluate Not condition" in {
      val rule = Rule(tradeAction, Condition.Not(Condition.NoPosition))
      Rule.findTriggeredAction(List(rule), trendChangedState, previousProfile) mustBe Some(tradeAction)
    }

    "evaluate UpperBandCrossed condition" in {
      val rule = Rule(tradeAction, Condition.UpperBandCrossed(Direction.Upward))
      Rule.findTriggeredAction(List(rule), upperBandCrossedState, previousProfile) mustBe Some(tradeAction)
    }

    "not trigger UpperBandCrossed when direction doesn't match" in {
      val rule = Rule(tradeAction, Condition.UpperBandCrossed(Direction.Downward))
      Rule.findTriggeredAction(List(rule), upperBandCrossedState, previousProfile) mustBe None
    }

    "not trigger UpperBandCrossed when it's not a fresh event" in {
      val prevProfile = MarketProfile.fromLegacy(
        MarketProfile.toLegacy(previousProfile).copy(lastBandCrossing = Some(BandCrossingState(Boundary.Upper, Direction.Upward, ts)))
      )
      val rule = Rule(tradeAction, Condition.UpperBandCrossed(Direction.Upward))
      Rule.findTriggeredAction(List(rule), upperBandCrossedState, prevProfile) mustBe None
    }

    "evaluate LowerBandCrossed condition" in {
      val rule = Rule(tradeAction, Condition.LowerBandCrossed(Direction.Downward))
      Rule.findTriggeredAction(List(rule), lowerBandCrossedState, previousProfile) mustBe Some(tradeAction)
    }

    "not trigger LowerBandCrossed when direction doesn't match" in {
      val rule = Rule(tradeAction, Condition.LowerBandCrossed(Direction.Upward))
      Rule.findTriggeredAction(List(rule), lowerBandCrossedState, previousProfile) mustBe None
    }

    "not trigger LowerBandCrossed when it's not a fresh event" in {
      val prevProfile = MarketProfile.fromLegacy(
        MarketProfile.toLegacy(previousProfile).copy(lastBandCrossing = Some(BandCrossingState(Boundary.Lower, Direction.Downward, ts)))
      )
      val rule = Rule(tradeAction, Condition.LowerBandCrossed(Direction.Downward))
      Rule.findTriggeredAction(List(rule), lowerBandCrossedState, prevProfile) mustBe None
    }

    "evaluate PriceCrossedLine condition" in {
      val rule = Rule(tradeAction, Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward))
      Rule.findTriggeredAction(List(rule), priceCrossedLineState, previousProfile) mustBe Some(tradeAction)
    }

    "not trigger PriceCrossedLine when line role doesn't match" in {
      val rule = Rule(tradeAction, Condition.PriceCrossedLine(ValueRole.Momentum, Direction.Upward))
      Rule.findTriggeredAction(List(rule), priceCrossedLineState, previousProfile) mustBe None
    }

    "not trigger PriceCrossedLine when direction doesn't match" in {
      val rule = Rule(tradeAction, Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Downward))
      Rule.findTriggeredAction(List(rule), priceCrossedLineState, previousProfile) mustBe None
    }

    "not trigger PriceCrossedLine when it's not a fresh event" in {
      val prevProfile =
        MarketProfile.fromLegacy(
          MarketProfile
            .toLegacy(previousProfile)
            .copy(lastPriceLineCrossing = Some(PriceLineCrossingState(ValueRole.ChannelMiddleBand, Direction.Upward, ts)))
        )
      val rule = Rule(tradeAction, Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward))
      Rule.findTriggeredAction(List(rule), priceCrossedLineState, prevProfile) mustBe None
    }
  }
}
