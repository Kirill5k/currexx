package currexx.core.market

import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.{Boundary, Direction, ValueRole, VolatilityRegime}
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

final case class MarketProfile(
    trend: Option[TrendState] = None,
    crossover: Option[CrossoverState] = None,
    momentum: Option[MomentumState] = None,
    lastMomentumValue: Option[BigDecimal] = None,
    volatility: Option[VolatilityState] = None,
    lastVolatilityValue: Option[BigDecimal] = None,
    lastVelocityValue: Option[BigDecimal] = None,
    lastBandCrossing: Option[BandCrossingState] = None,
    lastChannelMiddleBandValue: Option[BigDecimal] = None,
    lastPriceLineCrossing: Option[PriceLineCrossingState] = None
) derives Codec.AsObject


final case class PositionState(
    position: TradeOrder.Position,
    openedAt: Instant
) derives Codec.AsObject

final case class MarketState(
    userId: UserId,
    currencyPair: CurrencyPair,
    currentPosition: Option[PositionState],
    profile: MarketProfile,
    lastUpdatedAt: Instant,
    createdAt: Instant
) derives Codec.AsObject
