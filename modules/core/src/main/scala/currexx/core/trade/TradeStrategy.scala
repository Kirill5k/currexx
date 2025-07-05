package currexx.core.trade

import currexx.core.market.{MarketProfile, MarketState, MomentumZone}
import currexx.core.trade
import currexx.domain.JsonCodecs
import currexx.domain.signal.{Direction, VolatilityRegime}
import currexx.domain.types.EnumType
import io.circe.Codec
import org.latestbit.circe.adt.codec.*
import kirill5k.common.syntax.time.*

import scala.concurrent.duration.FiniteDuration

/** Represents a complete, self-contained trading strategy.
  *
  * @param openRules
  *   A list of rules that can trigger the opening of a new position. The first rule that evaluates to true will be acted upon.
  * @param closeRules
  *   A list of rules that can trigger the closing of an existing position. The first rule that evaluates to true will be acted upon.
  */
final case class TradeStrategy(
    openRules: List[Rule],
    closeRules: List[Rule]
) derives Codec.AsObject

object TradeAction extends EnumType[TradeAction](() => TradeAction.values)
enum TradeAction:
  case OpenLong, FlipToLong, OpenShort, FlipToShort, ClosePosition

/** A rule that pairs a set of conditions with a specific trading action.
  *
  * @param action
  *   The action to take if the conditions are met.
  * @param conditions
  *   The logical condition that must evaluate to true to trigger the action. This is the "entry" or "exit" criteria.
  */
final case class Rule(
    action: TradeAction,
    conditions: Rule.Condition
) derives Codec.AsObject

object Rule extends JsonCodecs {

  enum Condition(val kind: String) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
    case AllOf(conditions: List[Condition])        extends Condition("all-of")
    case AnyOf(conditions: List[Condition])        extends Condition("any-of")
    case Not(condition: Condition)                 extends Condition("not")
    case TrendChangedTo(direction: Direction)      extends Condition("trend-changed-to")
    case TrendIs(direction: Direction)             extends Condition("trend-is")
    case TrendActiveFor(duration: FiniteDuration)  extends Condition("trend-active-for")
    case CrossoverOccurred(direction: Direction)   extends Condition("crossover-occurred")
    case MomentumEntered(zone: MomentumZone)       extends Condition("momentum-entered")
    case MomentumIsIn(zone: MomentumZone)          extends Condition("momentum-is-in")
    case MomentumIs(direction: Direction)          extends Condition("momentum-is")
    case VolatilityIs(regime: VolatilityRegime)    extends Condition("volatility-is")
    case PositionIsOpen                            extends Condition("position-is-open")
    case NoPosition                                extends Condition("no-position")
    case PositionOpenFor(duration: FiniteDuration) extends Condition("position-open-for")

  object Condition:
    given JsonTaggedAdt.Config[Condition] = JsonTaggedAdt.Config.Values[Condition](
      mappings = Map(
        "all-of"             -> JsonTaggedAdt.tagged[Condition.AllOf],
        "any-of"             -> JsonTaggedAdt.tagged[Condition.AnyOf],
        "not"                -> JsonTaggedAdt.tagged[Condition.Not],
        "trend-changed-to"   -> JsonTaggedAdt.tagged[Condition.TrendChangedTo],
        "trend-is"           -> JsonTaggedAdt.tagged[Condition.TrendIs],
        "trend-active-for"   -> JsonTaggedAdt.tagged[Condition.TrendActiveFor],
        "crossover-occurred" -> JsonTaggedAdt.tagged[Condition.CrossoverOccurred],
        "momentum-entered"   -> JsonTaggedAdt.tagged[Condition.MomentumEntered],
        "momentum-is-in"     -> JsonTaggedAdt.tagged[Condition.MomentumIsIn],
        "momentum-is"        -> JsonTaggedAdt.tagged[Condition.MomentumIs],
        "volatility-is"      -> JsonTaggedAdt.tagged[Condition.VolatilityIs],
        "position-is-open"   -> JsonTaggedAdt.tagged[Condition.PositionIsOpen.type],
        "no-position"        -> JsonTaggedAdt.tagged[Condition.NoPosition.type],
        "position-open-for"  -> JsonTaggedAdt.tagged[Condition.PositionOpenFor]
      ),
      strict = true,
      typeFieldName = "kind"
    )

  def findTriggeredAction(
      rules: List[Rule],
      state: MarketState,
      previousProfile: MarketProfile
  ): Option[TradeAction] =
    rules.collectFirst {
      case rule if isSatisfied(rule.conditions, state, previousProfile) => rule.action
    }

  private def isSatisfied(
      condition: Rule.Condition,
      state: MarketState,
      previousProfile: MarketProfile
  ): Boolean = {
    val currentProfile = state.profile
    condition match {
      case Condition.AllOf(conditions) =>
        conditions.forall(c => isSatisfied(c, state, previousProfile))

      case Condition.AnyOf(conditions) =>
        conditions.exists(c => isSatisfied(c, state, previousProfile))

      case Condition.Not(c) =>
        !isSatisfied(c, state, previousProfile)

      case Condition.TrendChangedTo(direction) =>
        val currentDirection  = currentProfile.trend.map(_.direction)
        val previousDirection = previousProfile.trend.map(_.direction)
        currentDirection.contains(direction) && previousDirection != currentDirection

      case Condition.TrendIs(direction) =>
        currentProfile.trend.exists(_.direction == direction)

      case Condition.TrendActiveFor(duration) =>
        currentProfile.trend.exists { trendState =>
          val trendAge = trendState.confirmedAt.durationBetween(state.lastUpdatedAt)
          trendAge.toNanos >= duration.toNanos
        }

      case Condition.CrossoverOccurred(direction) =>
        val currentCrossover  = currentProfile.crossover
        val previousCrossover = previousProfile.crossover
        currentCrossover.exists(_.direction == direction) && currentCrossover != previousCrossover

      case Condition.MomentumEntered(zone) =>
        val currentZone  = currentProfile.momentum.map(_.zone)
        val previousZone = previousProfile.momentum.map(_.zone)
        // True if momentum is in the specified zone now, and was in a different zone before.
        currentZone.contains(zone) && previousZone != currentZone

      case Condition.MomentumIsIn(zone) =>
        currentProfile.momentum.exists(_.zone == zone)

      case Condition.MomentumIs(direction) =>
        (currentProfile.lastMomentumValue, previousProfile.lastMomentumValue) match {
          case (Some(current), Some(previous)) =>
            direction match {
              case Direction.Upward   => current > previous
              case Direction.Downward => current < previous
              case Direction.Still    => current == previous
            }
          case _ => false // Cannot determine direction without two consecutive values.
        }

      case Condition.VolatilityIs(regime) =>
        currentProfile.volatility.exists(_.regime == regime)

      case Condition.PositionIsOpen =>
        state.currentPosition.isDefined

      case Condition.NoPosition =>
        state.currentPosition.isEmpty

      case Condition.PositionOpenFor(duration) =>
        state.currentPosition.exists { pos =>
          val positionAge = pos.openedAt.durationBetween(state.lastUpdatedAt)
          positionAge.toNanos >= duration.toNanos
        }
    }
  }
}
