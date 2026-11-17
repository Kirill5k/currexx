package currexx.core.trade

import currexx.core.market.{MarketProfile, MarketState, MomentumZone}
import currexx.core.trade
import currexx.domain.JsonCodecs
import currexx.domain.market.TradeOrder
import currexx.domain.signal.{Boundary, Direction, IndicatorId, ValueRole, VolatilityRegime}
import currexx.domain.types.EnumType
import io.circe.{Codec, Decoder, Encoder}
import io.circe.derivation.{Configuration, ConfiguredCodec}
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

  object Operator extends EnumType[Operator](() => Operator.values)
  enum Operator:
    case GreaterThan, LessThan, EqualTo

  enum Condition:
    case AllOf(conditions: List[Condition])
    case AnyOf(conditions: List[Condition])
    case Not(condition: Condition)
    case TrendChangedTo(direction: Direction, of: Option[IndicatorId] = None)
    case TrendIs(direction: Direction, of: Option[IndicatorId] = None)
    case TrendActiveFor(duration: FiniteDuration, of: Option[IndicatorId] = None)
    case CrossoverOccurred(direction: Direction, of: Option[IndicatorId] = None)
    case MomentumEntered(zone: MomentumZone, of: Option[IndicatorId] = None)
    case MomentumIsIn(zone: MomentumZone, of: Option[IndicatorId] = None)
    case MomentumIs(direction: Direction, of: Option[IndicatorId] = None)
    case PreviousVolatilityIs(regime: VolatilityRegime, of: Option[IndicatorId] = None)
    case VolatilityIs(regime: VolatilityRegime, of: Option[IndicatorId] = None)
    case VelocityIs(direction: Direction, of: Option[IndicatorId] = None)
    case VelocityCrossedLevel(level: Double, direction: Direction, of: Option[IndicatorId] = None)
    case VelocityIsBelow(threshold: Double, of: Option[IndicatorId] = None)
    case PositionIs(position: TradeOrder.Position)
    case PositionOpenFor(duration: FiniteDuration)
    case NoPosition
    case UpperBandCrossed(direction: Direction, of: Option[IndicatorId] = None)
    case LowerBandCrossed(direction: Direction, of: Option[IndicatorId] = None)
    case PriceCrossedLine(lineRole: ValueRole, direction: Direction, of: Option[IndicatorId] = None)
    case ValueIs(role: ValueRole, operator: Operator, level: Double, of: Option[IndicatorId] = None)
    // Price-distance stop: true when price has moved at least `nAtr` * ATR against the open
    // position. Requires raw-close and ATR readings, and the position to carry its entry price (openPrice).
    case PriceMovedAgainstEntry(nAtr: Double, priceOf: Option[IndicatorId] = None, atrOf: Option[IndicatorId] = None)

  object Condition:
    given Configuration = Configuration.default
      .withDiscriminator("kind")
      .withKebabCaseConstructorNames

    private lazy val derivedCodec: Codec.AsObject[Condition] = ConfiguredCodec.derived[Condition]
    given Codec.AsObject[Condition]                          = Codec.AsObject.from(
      Decoder.instance(c => derivedCodec(c)),
      Encoder.AsObject.instance(c =>
        derivedCodec.encodeObject(c).filter { case (key, value) =>
          !Set("of", "priceOf", "atrOf").contains(key) || !value.isNull
        }
      )
    )

    val trendIsUpward: Condition             = Condition.TrendIs(Direction.Upward)
    val trendIsDownward: Condition           = Condition.TrendIs(Direction.Downward)
    val positionIsBuy: Condition             = Condition.PositionIs(TradeOrder.Position.Buy)
    val positionIsSell: Condition            = Condition.PositionIs(TradeOrder.Position.Sell)
    val downwardCrossover: Condition         = Condition.CrossoverOccurred(Direction.Downward)
    val upwardCrossover: Condition           = Condition.CrossoverOccurred(Direction.Upward)
    val volatilityIsHigh: Condition          = Condition.VolatilityIs(VolatilityRegime.High)
    val volatilityIsLow: Condition           = Condition.VolatilityIs(VolatilityRegime.Low)
    val momentumEnteredOverbought: Condition = Condition.MomentumEntered(MomentumZone.Overbought)
    val momentumEnteredOversold: Condition   = Condition.MomentumEntered(MomentumZone.Oversold)
    val momentumIsInOverbought: Condition    = Condition.MomentumIsIn(MomentumZone.Overbought)
    val momentumIsInOversold: Condition      = Condition.MomentumIsIn(MomentumZone.Oversold)

    def not(condition: Condition): Condition     = Condition.Not(condition)
    def allOf(conditions: Condition*): Condition = Condition.AllOf(conditions.toList)
    def anyOf(conditions: Condition*): Condition = Condition.AnyOf(conditions.toList)

  def findTriggeredAction(
      rules: List[Rule],
      state: MarketState,
      previousProfile: MarketProfile
  ): Option[TradeAction] =
    if state.invalidated.isEmpty then
      rules.collectFirst { case rule if isSatisfied(rule.conditions, state, previousProfile) => rule.action }
    else rules.collectFirst { case rule if evaluate(rule.conditions, state, previousProfile).contains(true) => rule.action }

  private def evaluate(
      condition: Rule.Condition,
      state: MarketState,
      previousProfile: MarketProfile
  ): Option[Boolean] =
    condition match
      case Condition.AllOf(cs) => combine(cs.iterator.map(evaluate(_, state, previousProfile)), decisive = false)
      case Condition.AnyOf(cs) => combine(cs.iterator.map(evaluate(_, state, previousProfile)), decisive = true)
      case Condition.Not(c)    => evaluate(c, state, previousProfile).map(!_)
      case leaf                => Option.unless(unavailable(leaf, state))(isSatisfied(leaf, state, previousProfile))

  @scala.annotation.tailrec
  private def combine(values: Iterator[Option[Boolean]], decisive: Boolean, unknown: Boolean = false): Option[Boolean] =
    if !values.hasNext then Option.unless(unknown)(!decisive)
    else
      values.next() match
        case Some(value) if value == decisive => Some(decisive)
        case value                            => combine(values, decisive, unknown || value.isEmpty)

  private def unavailable(condition: Condition, state: MarketState): Boolean =
    val noPositionStop = condition.isInstanceOf[Condition.PriceMovedAgainstEntry] && state.currentPosition.isEmpty
    !noPositionStop && StrategyCheck.inputs(condition).exists { input =>
      state.invalidated.contains(input.id) && (input.timing match
        case StrategyCheck.Timing.State   => !state.profile.readings.contains(input.id)
        case StrategyCheck.Timing.History => true
        case StrategyCheck.Timing.Event   => false)
    }

  private def isSatisfied(condition: Condition, state: MarketState, previousProfile: MarketProfile): Boolean = {
    val currentProfile = state.profile
    condition match {
      case Condition.AllOf(cs)                               => cs.forall(isSatisfied(_, state, previousProfile))
      case Condition.AnyOf(cs)                               => cs.exists(isSatisfied(_, state, previousProfile))
      case Condition.Not(c)                                  => !isSatisfied(c, state, previousProfile)
      case Rule.Condition.ValueIs(role, operator, level, of) =>
        val valueToTest = currentProfile.number(of.getOrElse(IndicatorId.forValue(role)))
        valueToTest.exists { value =>
          val bdLevel = BigDecimal.valueOf(level)
          operator match
            case Operator.GreaterThan => value > bdLevel
            case Operator.LessThan    => value < bdLevel
            case Operator.EqualTo     => value == bdLevel
        }

      case Condition.TrendChangedTo(direction, of) =>
        val id                = of.getOrElse(IndicatorId.Trend)
        val currentDirection  = currentProfile.trend(id).map(_.direction)
        val previousDirection = previousProfile.trend(id).map(_.direction)
        currentDirection.contains(direction) && previousDirection != currentDirection

      case Condition.TrendIs(direction, of) =>
        currentProfile.trend(of.getOrElse(IndicatorId.Trend)).exists(_.direction == direction)

      case Condition.TrendActiveFor(duration, of) =>
        currentProfile.trend(of.getOrElse(IndicatorId.Trend)).exists { trendState =>
          val trendAge = trendState.confirmedAt.durationBetween(state.lastUpdatedAt)
          trendAge.toNanos >= duration.toNanos
        }

      case Condition.CrossoverOccurred(direction, of) =>
        val id                = of.getOrElse(IndicatorId.Crossover)
        val currentCrossover  = currentProfile.crossover(id)
        val previousCrossover = previousProfile.crossover(id)
        currentCrossover.exists(_.direction == direction) && currentCrossover != previousCrossover

      case Condition.MomentumEntered(zone, of) =>
        val id           = of.getOrElse(IndicatorId.MomentumZone)
        val currentZone  = currentProfile.momentum(id).map(_.zone)
        val previousZone = previousProfile.momentum(id).map(_.zone)
        // True if momentum is in the specified zone now, and was in a different zone before.
        currentZone.contains(zone) && previousZone != currentZone

      case Condition.MomentumIsIn(zone, of) =>
        currentProfile.momentum(of.getOrElse(IndicatorId.MomentumZone)).exists(_.zone == zone)

      case Condition.MomentumIs(direction, of) =>
        val id = of.getOrElse(IndicatorId.forValue(ValueRole.Momentum))
        (
          currentProfile.number(id),
          previousProfile.number(id)
        ) match {
          case (Some(current), Some(previous)) =>
            direction match {
              case Direction.Upward   => current > previous
              case Direction.Downward => current < previous
              case Direction.Still    => current == previous
            }
          case _ => false // Cannot determine direction without two consecutive values.
        }

      case Condition.PreviousVolatilityIs(regime, of) =>
        previousProfile.volatility(of.getOrElse(IndicatorId.VolatilityRegime)).exists(_.regime == regime)

      case Condition.VolatilityIs(regime, of) =>
        currentProfile.volatility(of.getOrElse(IndicatorId.VolatilityRegime)).exists(_.regime == regime)

      case Condition.PositionIs(position) =>
        state.currentPosition.exists(_.position == position)

      case Condition.NoPosition =>
        state.currentPosition.isEmpty

      case Condition.PositionOpenFor(duration) =>
        state.currentPosition.exists { pos =>
          val positionAge = pos.openedAt.durationBetween(state.lastUpdatedAt)
          positionAge.toNanos >= duration.toNanos
        }

      case Condition.VelocityIs(direction, of) =>
        currentProfile.number(of.getOrElse(IndicatorId.forValue(ValueRole.Velocity))) match {
          case Some(v) =>
            val epsilon = 1e-10
            direction match {
              case Direction.Upward   => v > epsilon
              case Direction.Downward => v < -epsilon
              case Direction.Still    => v.abs <= epsilon
            }
          case None => false // Cannot determine if velocity is not present.
        }

      case Condition.VelocityIsBelow(threshold, of) =>
        currentProfile.number(of.getOrElse(IndicatorId.forValue(ValueRole.Velocity))).exists(velocity => velocity.abs < threshold)

      case Condition.VelocityCrossedLevel(level, direction, of) =>
        val id = of.getOrElse(IndicatorId.forValue(ValueRole.Velocity))
        // This requires both the current and previous velocity values.
        (
          currentProfile.number(id),
          previousProfile.number(id)
        ) match {
          case (Some(currentValue), Some(previousValue)) =>
            direction match {
              case Direction.Upward   => currentValue >= level && previousValue < level
              case Direction.Downward => currentValue <= level && previousValue > level
              case Direction.Still    => false
            }
          case _ => false // Cannot determine a cross without two consecutive points.
        }

      case Condition.UpperBandCrossed(direction, of) =>
        val id = of.getOrElse(IndicatorId.Band)
        // The condition is met if:
        // 1. A band crossing event exists.
        // 2. It was an UPPER band crossing.
        // 3. The direction matches the rule.
        // 4. AND it's a "fresh" event (different from the previous one).
        currentProfile
          .band(id)
          .exists(cross => cross.boundary == Boundary.Upper && cross.direction == direction)
        && currentProfile.band(id) != previousProfile.band(id)

      case Condition.LowerBandCrossed(direction, of) =>
        val id = of.getOrElse(IndicatorId.Band)
        currentProfile
          .band(id)
          .exists(cross => cross.boundary == Boundary.Lower && cross.direction == direction)
        && currentProfile.band(id) != previousProfile.band(id)

      case Condition.PriceCrossedLine(lineRole, direction, of) =>
        val id = of.getOrElse(IndicatorId.forLine(lineRole))
        currentProfile.crossover(id).exists(_.direction == direction)
        && currentProfile.crossover(id) != previousProfile.crossover(
          id
        )

      case Condition.PriceMovedAgainstEntry(nAtr, priceOf, atrOf) =>
        (
          state.currentPosition,
          currentProfile.number(priceOf.getOrElse(IndicatorId.forValue(ValueRole.Price))),
          currentProfile.number(atrOf.getOrElse(IndicatorId.forValue(ValueRole.Volatility)))
        ) match {
          case (Some(pos), Some(currentPrice), Some(atr)) =>
            pos.openPrice.exists { entryPrice =>
              // Adverse excursion: how far price has moved against the position since entry.
              val adverseMove = pos.position match
                case TradeOrder.Position.Buy  => entryPrice - currentPrice // a long loses as price falls
                case TradeOrder.Position.Sell => currentPrice - entryPrice // a short loses as price rises
              adverseMove >= (BigDecimal(nAtr) * atr)
            }
          case _ => false // need position (with entry price), current price and ATR to evaluate
        }

    }
  }
}
