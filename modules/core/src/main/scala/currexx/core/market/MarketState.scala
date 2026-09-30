package currexx.core.market

import currexx.core.signal.Signal
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.{Boundary, Direction, ValueRole, VolatilityRegime}
import currexx.domain.user.UserId
import currexx.domain.types.EnumType
import io.circe.Codec
import kirill5k.common.syntax.time.*

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

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
    lastPriceLineCrossing: Option[PriceLineCrossingState] = None,
    // Trend strength (e.g. ADX) tracked in its own slot so it does NOT collide with the shared
    // momentum zone that oscillator ThresholdCrossings write to. Read via ValueIs(TrendStrength, ...).
    lastTrendStrengthValue: Option[BigDecimal] = None,
    // Latest source price, tracked so price-distance stops (PriceMovedAgainstEntry) can be evaluated.
    lastClosePrice: Option[BigDecimal] = None
) derives Codec.AsObject:
  // Only state confirmed before the market reopened is shifted or cleared; anything newer already reflects the reopened market.
  def adjustForMarketClosure(gap: FiniteDuration, reopenedAt: Instant): MarketProfile = {
    def shift(time: Instant): Instant = if (time.isBefore(reopenedAt)) time.plus(gap) else time
    def keep(time: Instant): Boolean  = !time.isBefore(reopenedAt)
    copy(
      trend = trend.map(s => s.copy(confirmedAt = shift(s.confirmedAt))),
      momentum = momentum.map(s => s.copy(confirmedAt = shift(s.confirmedAt))),
      volatility = volatility.map(s => s.copy(confirmedAt = shift(s.confirmedAt))),
      crossover = crossover.filter(s => keep(s.confirmedAt)),
      lastBandCrossing = lastBandCrossing.filter(s => keep(s.confirmedAt)),
      lastPriceLineCrossing = lastPriceLineCrossing.filter(s => keep(s.confirmedAt))
    )
  }

final case class PositionState(
    position: TradeOrder.Position,
    openedAt: Instant,
    // Entry price of the open position as reported by the broker; used by price-distance stops. Optional for codec
    // backward-compatibility with states persisted before this field existed.
    openPrice: Option[BigDecimal] = None
) derives Codec.AsObject

final case class MarketState(
    userId: UserId,
    currencyPair: CurrencyPair,
    currentPosition: Option[PositionState],
    profile: MarketProfile,
    lastUpdatedAt: Instant,
    createdAt: Instant,
    previousProfile: Option[MarketProfile] = None,
    // Processing cursors are independent of the state timestamps adjusted for market closures.
    lastCandleTime: Option[Instant] = None,
    lastTimeStateCandle: Option[Instant] = None,
    // Optimistic-locking version, None until the state is first stored: every write must match it and increments it.
    version: Option[Long] = None
) derives Codec.AsObject:
  def adjustForMarketClosure(gap: FiniteDuration, reopenedAt: Instant): MarketState =
    copy(
      profile = profile.adjustForMarketClosure(gap, reopenedAt),
      currentPosition = currentPosition.map(p => if (p.openedAt.isBefore(reopenedAt)) p.copy(openedAt = p.openedAt.plus(gap)) else p)
    )

  // None means the candle has already been applied (or is older than one that has).
  def applyCandleSignals(signals: List[Signal], candleTime: Instant): Option[MarketState] =
    Option.when(lastCandleTime.forall(_.isBefore(candleTime))) {
      withProfile(signals.foldLeft(profile)(MarketProfileUpdater.update)).copy(lastCandleTime = Some(candleTime))
    }

  def applyManualSignal(signal: Signal): Option[MarketState] =
    Some(withProfile(MarketProfileUpdater.update(profile, signal))).filter(_.profile != profile)

  def applyTimeState(candleTime: Instant, marketClosureGap: Option[FiniteDuration]): Option[MarketState] =
    Option.when(lastTimeStateCandle.forall(_.isBefore(candleTime))) {
      marketClosureGap.fold(this)(adjustForMarketClosure(_, candleTime)).copy(lastTimeStateCandle = Some(candleTime))
    }

  private def withProfile(updated: MarketProfile): MarketState =
    if (updated == profile) this else copy(profile = updated, previousProfile = Some(profile))

object MarketState:
  def initial(uid: UserId, cp: CurrencyPair, now: Instant): MarketState =
    MarketState(uid, cp, None, MarketProfile(), now, now)
