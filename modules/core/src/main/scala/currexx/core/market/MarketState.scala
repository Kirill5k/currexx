package currexx.core.market

import currexx.domain.market.{CurrencyPair, Interval, TradeOrder}
import currexx.domain.signal.{Indicator, IndicatorId}
import currexx.domain.user.UserId
import currexx.domain.types.EnumType
import io.circe.Codec
import kirill5k.common.syntax.time.*

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

object MomentumZone extends EnumType[MomentumZone](() => MomentumZone.values)
enum MomentumZone:
  case Overbought, Oversold, Neutral

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
    version: Option[Long] = None,
    configRevision: Option[Long] = None,
    interval: Option[Interval] = None,
    invalidated: Set[IndicatorId] = Set.empty,
    sources: Map[IndicatorId, Indicator] = Map.empty
) derives Codec.AsObject:
  def adjustForMarketClosure(gap: FiniteDuration, reopenedAt: Instant): MarketState =
    copy(
      profile = profile.adjustForMarketClosure(gap, reopenedAt),
      currentPosition = currentPosition.map(p => if (p.openedAt.isBefore(reopenedAt)) p.copy(openedAt = p.openedAt.plus(gap)) else p)
    )

object MarketState:
  def initial(uid: UserId, cp: CurrencyPair, now: Instant): MarketState =
    MarketState(uid, cp, None, MarketProfile(), now, now)
