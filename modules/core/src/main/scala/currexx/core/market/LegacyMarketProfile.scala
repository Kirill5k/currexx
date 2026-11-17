package currexx.core.market

import io.circe.Codec
import java.time.Instant
import currexx.domain.signal.{Boundary, Direction, ValueRole, VolatilityRegime}
import LegacyMarketProfile.*

/** Wire representation used only for legacy decoding and rollback mirrors. */
final case class LegacyMarketProfile(
    trend: Option[TrendState] = None,
    crossover: Option[CrossoverState] = None,
    momentum: Option[MomentumState] = None,
    lastMomentumValue: Option[BigDecimal] = None,
    volatility: Option[VolatilityState] = None,
    lastVolatilityValue: Option[BigDecimal] = None,
    lastVelocityValue: Option[BigDecimal] = None,
    lastBandCrossing: Option[BandCrossingState] = None,
    lastChannelMiddleBandValue: Option[BigDecimal] = None,
    lastPriceLineCrossing: Option[PriceLineCrossingState] = None,
    // Trend strength (e.g. ADX) tracked in its own slot so it does NOT collide with the shared
    // momentum zone that oscillator ThresholdCrossings write to. Read via ValueIs(TrendStrength, ...).
    lastTrendStrengthValue: Option[BigDecimal] = None,
    // Latest source price, tracked so price-distance stops (PriceMovedAgainstEntry) can be evaluated.
    lastClosePrice: Option[BigDecimal] = None
) derives Codec.AsObject

object LegacyMarketProfile:
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

  final case class BandCrossingState(
      boundary: Boundary,
      direction: Direction,
      confirmedAt: Instant
  ) derives Codec.AsObject

  final case class PriceLineCrossingState(
      role: ValueRole,
      direction: Direction,
      confirmedAt: Instant
  ) derives Codec.AsObject
