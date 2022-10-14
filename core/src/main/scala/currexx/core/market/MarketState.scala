package currexx.core.market

import currexx.domain.market.{Condition, CurrencyPair, Indicator, IndicatorKind, PriceRange, TradeOrder}
import currexx.domain.user.UserId
import io.circe.Codec

import java.time.Instant

final case class IndicatorState(
    condition: Condition,
    time: Instant,
    triggeredBy: Indicator
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
    signals: Map[IndicatorKind, List[IndicatorState]],
    lastUpdatedAt: Option[Instant],
    createdAt: Option[Instant]
) derives Codec.AsObject
