package currexx.core.market

import currexx.core.signal.Signal
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.{Boundary, Condition, Direction, VolatilityRegime}
import currexx.domain.user.UserId
import currexx.domain.types.EnumType
import io.circe.Codec

import java.time.Instant

object MomentumZone extends EnumType[MomentumZone](() => MomentumZone.values)
enum MomentumZone:
  case Overbought, Oversold, Neutral

final case class TrendState(
    direction: Direction,
    confirmedAt: Instant
) derives Codec.AsObject

final case class CrossoverState(
    direction: Direction,
    confirmedAt: Instant
) derives Codec.AsObject

final case class MomentumState(
    zone: MomentumZone,
    confirmedAt: Instant
) derives Codec.AsObject

final case class VolatilityState(
    regime: VolatilityRegime,
    confirmedAt: Instant
) derives Codec.AsObject

final case class MarketProfile(
    trend: Option[TrendState] = None,
    crossover: Option[CrossoverState] = None,
    momentum: Option[MomentumState] = None,
    lastMomentumValue: Option[BigDecimal] = None,
    volatility: Option[VolatilityState] = None,
    lastVolatilityValue: Option[BigDecimal] = None
) derives Codec.AsObject

object MarketProfile {
  extension (profile: MarketProfile)
    def update(signal: Signal): MarketProfile =
      signal.condition match {
        // --- Trend Signal ---
        case Condition.TrendDirectionChange(_, to, _) =>
          // A trend change occurred. Create a new TrendState.
          val currentTrendDirection = profile.trend.map(_.direction)
          if (currentTrendDirection.isEmpty || !currentTrendDirection.contains(to)) {
            val newTrendState = TrendState(
              direction = to,
              confirmedAt = signal.time
            )
            profile.copy(trend = Some(newTrendState))
          } else profile

        // --- Crossover Signal ---
        case Condition.LinesCrossing(direction) =>
          // A crossover occurred. Create a new CrossoverState.
          val currentCrossoverDirection = profile.crossover.map(_.direction)
          if (currentCrossoverDirection.isEmpty || !currentCrossoverDirection.contains(direction)) {
            val newCrossoverState = CrossoverState(
              direction = direction,
              confirmedAt = signal.time
            )
            profile.copy(crossover = Some(newCrossoverState))
          } else profile

        case Condition.ThresholdCrossing(_, value, direction, boundary) =>
          // Determine the new zone based on the signal's direction and boundary
          val newZone = (direction, boundary) match {
            case (Direction.Upward, Boundary.Upper)   => MomentumZone.Overbought
            case (Direction.Downward, Boundary.Lower) => MomentumZone.Oversold
            case (Direction.Downward, Boundary.Upper) => MomentumZone.Neutral
            case (Direction.Upward, Boundary.Lower)   => MomentumZone.Neutral
            case _                                    =>
              // This case (e.g., Still direction) shouldn't happen, but we can handle it
              // by just taking the current zone from the profile.
              profile.momentum.map(_.zone).getOrElse(MomentumZone.Neutral)
          }

          // Check if the zone has actually changed from the previous state.
          val currentZone = profile.momentum.map(_.zone)
          if (!currentZone.contains(newZone)) {
            // The zone has changed. Create a new MomentumState with the new zone
            // and the current signal's timestamp as its `confirmedAt` time.
            val newMomentumState = MomentumState(zone = newZone, confirmedAt = signal.time)
            profile.copy(momentum = Some(newMomentumState), lastMomentumValue = Some(value))
          } else {
            // The zone is the same, but we should still update the latest raw value.
            profile.copy(lastMomentumValue = Some(value))
          }

        case Condition.VolatilityRegimeChanged(_, to) =>
          val currentRegime = profile.volatility.map(_.regime)
          if (!currentRegime.contains(to)) {
            val newVolatilityState = VolatilityState(
              regime = to,
              confirmedAt = signal.time
            )
            // Note: This assumes another mechanism will update `lastVolatilityValue`.
            // A good way is to have the VolatilityRegimeChanged signal also carry the raw ATR value.
            // Let's assume for now the signal is simple.
            profile.copy(volatility = Some(newVolatilityState))
          } else profile

        // --- Volatility Signals (e.g., from Keltner Channel) ---
        // Here we just pass the condition through. A more advanced system might create a VolatilityState.
        case _ @(_: Condition.UpperBandCrossing | _: Condition.LowerBandCrossing) =>
          // This part of the profile could be enhanced further, but for now, we just note the event.
          // Let's assume the `VolatilityState` logic is not yet fully implemented.
          // For now, we can just update the last known value from a related indicator if available.
          // This part is highly dependent on how you define volatility signals.
          // As a placeholder, we do nothing with the state, just the value if we had it.
          profile // No change to the profile state, maybe just update a raw value if one was passed.

        // --- Composite Signal ---
        case Condition.Composite(conditions) =>
          // A composite signal is just a bundle of other signals.
          // We recursively fold over its inner conditions to update the profile.
          // This ensures that if a composite contains a TrendChange and a ThresholdCrossing,
          // BOTH the `trend` and `momentum` fields of the profile get updated correctly.
          conditions.foldLeft(profile) { (currentProfile, innerCondition) =>
            // To properly update, the inner call also needs the signal context.
            // This highlights a design choice: for simplicity here, we assume the composite's
            // top-level signal context (like time) applies to all children.
            currentProfile.update(signal.copy(condition = innerCondition))
          }
      }
}

final case class PositionState(
    position: TradeOrder.Position,
    openedAt: Instant,
    openPrice: BigDecimal
) derives Codec.AsObject

final case class MarketState(
    userId: UserId,
    currencyPair: CurrencyPair,
    currentPosition: Option[PositionState],
    profile: MarketProfile,
    lastUpdatedAt: Instant,
    createdAt: Instant
) derives Codec.AsObject
