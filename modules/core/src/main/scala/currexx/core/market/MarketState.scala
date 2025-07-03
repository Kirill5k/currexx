package currexx.core.market

import currexx.core.signal.Signal
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.{Condition, Direction}
import currexx.domain.user.UserId
import currexx.domain.types.EnumType
import io.circe.Codec

import java.time.Instant

object MomentumZone extends EnumType[MomentumZone](() => MomentumZone.values)
enum MomentumZone:
  case Overbought, Oversold, Neutral

object VolatilityRegime extends EnumType[VolatilityRegime](() => VolatilityRegime.values)
enum VolatilityRegime:
  case High, Low, Expanding, Contracting

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
    lastMomentumValue: Option[Double] = None,
    volatility: Option[VolatilityState] = None,
    lastVolatilityValue: Option[Double] = None
) derives Codec.AsObject

object MarketProfile {
  extension (profile: MarketProfile)
    def update(signal: Signal): MarketProfile =
      signal.condition match {
        // --- Trend Signal ---
        case Condition.TrendDirectionChange(_, to, _) =>
          // A trend change occurred. Create a new TrendState.
          val newTrendState = TrendState(
            direction = to,
            confirmedAt = signal.time
          )
          profile.copy(trend = Some(newTrendState))

        // --- Crossover Signal ---
        case Condition.LinesCrossing(direction) =>
          // A crossover occurred. Create a new CrossoverState.
          val newCrossoverState = CrossoverState(
            direction = direction,
            confirmedAt = signal.time
          )
          profile.copy(crossover = Some(newCrossoverState))

        // --- Momentum Signals (Threshold Crossing) ---
        case Condition.AboveThreshold(_, value) =>
          // The oscillator crossed the upper boundary.
          // We are now in the Overbought zone.
          val newMomentumState = MomentumState(
            zone = MomentumZone.Overbought,
            confirmedAt = signal.time
          )
          profile.copy(
            momentum = Some(newMomentumState),
            lastMomentumValue = Some(value.toDouble) // Update the latest raw value
          )

        case Condition.BelowThreshold(_, value) =>
          // The oscillator crossed the lower boundary.
          // We are now in the Oversold zone.
          val newMomentumState = MomentumState(
            zone = MomentumZone.Oversold,
            confirmedAt = signal.time
          )
          profile.copy(
            momentum = Some(newMomentumState),
            lastMomentumValue = Some(value.toDouble) // Update the latest raw value
          )

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
