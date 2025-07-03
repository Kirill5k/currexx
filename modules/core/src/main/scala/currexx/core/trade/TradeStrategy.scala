package currexx.core.trade

import currexx.core.market.{MarketState, MomentumZone, VolatilityRegime}
import currexx.core.trade
import currexx.domain.signal.Direction
import currexx.domain.types.EnumType
import currexx.domain.json.given
import io.circe.Codec
import org.latestbit.circe.adt.codec.*

import scala.concurrent.duration.FiniteDuration

/** Represents a complete, self-contained trading strategy.
  *
  * @param openRules
  *   A list of rules that can trigger the opening of a new position. The first rule that evaluates to true will be acted upon.
  * @param closeRules
  *   A list of rules that can trigger the closing of an existing position. The first rule that evaluates to true will be acted upon.
  */
final case class TradeStrategy(
    openRules: List[ActionRule],
    closeRules: List[ActionRule]
) derives Codec.AsObject

object TradeAction extends EnumType[TradeAction](() => TradeAction.values)
enum TradeAction:
  case OpenLong, OpenShort, ClosePosition

/** A rule that pairs a set of conditions with a specific trading action.
  *
  * @param action
  *   The action to take if the conditions are met.
  * @param conditions
  *   The logical condition that must evaluate to true to trigger the action. This is the "entry" or "exit" criteria.
  */
final case class ActionRule(
    action: TradeAction,
    conditions: RuleCondition
) derives Codec.AsObject

enum RuleCondition(val kind: String) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  // --- Logical Combinators ---
  /** Evaluates to true if ALL of its child conditions are true. (Logical AND) */
  case AllOf(conditions: List[RuleCondition]) extends RuleCondition("all-of")

  /** Evaluates to true if AT LEAST ONE of its child conditions is true. (Logical OR) */
  case AnyOf(conditions: List[RuleCondition]) extends RuleCondition("any-of")

  /** Inverts the result of its child condition. (Logical NOT) */
  case Not(condition: RuleCondition) extends RuleCondition("not")

  // --- Atomic Conditions (The building blocks) ---
  case TrendChangedTo(direction: Direction)      extends RuleCondition("trend-changed-to")
  case TrendIs(direction: Direction)             extends RuleCondition("trend-is")         // e.g., trend is up or down
  case TrendActiveFor(duration: FiniteDuration)  extends RuleCondition("trend-active-for") // e.g., active for more than 24 hours
  case CrossoverOccurred(direction: Direction)   extends RuleCondition("crossover-occurred")
  case MomentumEntered(zone: MomentumZone)       extends RuleCondition("momentum-entered")
  case MomentumIsIn(zone: MomentumZone)          extends RuleCondition("momentum-is-in")
  case MomentumIs(direction: Direction)          extends RuleCondition("momentum-is")
  case VolatilityIs(regime: VolatilityRegime)    extends RuleCondition("volatility-is")
  case PositionIsOpen                            extends RuleCondition("position-is-open")
  case NoPosition                                extends RuleCondition("no-position")
  case PositionOpenFor(duration: FiniteDuration) extends RuleCondition("position-open-for")

object RuleCondition:
  given JsonTaggedAdt.Config[RuleCondition] = JsonTaggedAdt.Config.Values[RuleCondition](
    mappings = Map(
      "all-of"             -> JsonTaggedAdt.tagged[RuleCondition.AllOf],
      "any-of"             -> JsonTaggedAdt.tagged[RuleCondition.AnyOf],
      "not"                -> JsonTaggedAdt.tagged[RuleCondition.Not],
      "trend-changed-to"   -> JsonTaggedAdt.tagged[RuleCondition.TrendChangedTo],
      "trend-is"           -> JsonTaggedAdt.tagged[RuleCondition.TrendIs],
      "trend-active-for"   -> JsonTaggedAdt.tagged[RuleCondition.TrendActiveFor],
      "crossover-occurred" -> JsonTaggedAdt.tagged[RuleCondition.CrossoverOccurred],
      "momentum-entered"   -> JsonTaggedAdt.tagged[RuleCondition.MomentumEntered],
      "momentum-is-in"     -> JsonTaggedAdt.tagged[RuleCondition.MomentumIsIn],
      "momentum-is"        -> JsonTaggedAdt.tagged[RuleCondition.MomentumIs],
      "volatility-is"      -> JsonTaggedAdt.tagged[RuleCondition.VolatilityIs],
      "position-is-open"   -> JsonTaggedAdt.tagged[RuleCondition.PositionIsOpen.type],
      "no-position"        -> JsonTaggedAdt.tagged[RuleCondition.NoPosition.type],
      "position-open-for"  -> JsonTaggedAdt.tagged[RuleCondition.PositionOpenFor]
    ),
    strict = true,
    typeFieldName = "kind"
  )

//TODO:
object TradeStrategyExecutor {
  enum Decision:
    case Buy, Sell, Close

  private case object Disabled extends TradeStrategyExecutor:
    def analyze(state: MarketState): Option[TradeStrategyExecutor.Decision] = None

  def get(strategy: TradeStrategy): TradeStrategyExecutor = Disabled
}
