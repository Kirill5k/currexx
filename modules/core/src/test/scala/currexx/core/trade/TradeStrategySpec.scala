package currexx.core.trade

import currexx.core.fixtures.Markets.*
import currexx.core.market.{CrossoverState, MarketProfile, MarketState, MomentumState, MomentumZone, TrendState, VolatilityState}
import currexx.core.trade.Rule.Condition
import currexx.domain.signal.{Direction, VolatilityRegime}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.*

class TradeStrategySpec extends AnyWordSpec with Matchers {

  val tradeAction: TradeAction = TradeAction.OpenLong
  val anotherTradeAction: TradeAction = TradeAction.OpenShort
  val closePositionAction: TradeAction = TradeAction.ClosePosition

  val previousProfile: MarketProfile = profile.copy(lastMomentumValue = Some(10.0))
  val trendChangedState: MarketState = state.copy(
    profile = profile.copy(
      trend = Some(TrendState(Direction.Downward, ts)),
      crossover = Some(CrossoverState(Direction.Downward, ts)),
      momentum = Some(MomentumState(MomentumZone.Oversold, ts)),
      volatility = Some(VolatilityState(VolatilityRegime.High, ts)),
      lastMomentumValue = Some(20.0)
    )
  )
  val trendActiveForState: MarketState = trendChangedState.copy(
    profile = trendChangedState.profile.copy(
      trend = trendChangedState.profile.trend.map(_.copy(confirmedAt = ts.minusSeconds(100)))
    )
  )
  val momentumIsState: MarketState =
    trendChangedState.copy(profile = trendChangedState.profile.copy(lastMomentumValue = Some(20.0)))
  val positionOpenForState: MarketState = trendChangedState.copy(
    currentPosition = Some(positionState.copy(openedAt = ts.minusSeconds(70)))
  )
  val noPositionState: MarketState = trendChangedState.copy(currentPosition = None)
  val momentumStillState: MarketState =
    trendChangedState.copy(profile = trendChangedState.profile.copy(lastMomentumValue = Some(10.0)))
  val momentumEnteredState: MarketState = trendChangedState.copy(
    profile = trendChangedState.profile.copy(momentum = Some(MomentumState(MomentumZone.Overbought, ts)))
  )
  val crossoverOccurredState: MarketState = trendChangedState.copy(
    profile = trendChangedState.profile.copy(crossover = Some(CrossoverState(Direction.Downward, ts)))
  )

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
        Rule(tradeAction, Condition.PositionIsOpen)
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
      val rule = Rule(tradeAction, Condition.PositionIsOpen)
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
      val rule = Rule(closePositionAction, Condition.AllOf(List(Condition.PositionIsOpen, Condition.TrendIs(Direction.Downward))))
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
  }
}
