package currexx.core.market

import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.Direction
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
