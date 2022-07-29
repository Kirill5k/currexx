package currexx.core.market

import currexx.domain.market.{Condition, CurrencyPair, Indicator, PriceRange, TradeOrder}
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
    price: PriceRange
) derives Codec.AsObject

final case class MarketState(
    userId: UserId,
    currencyPair: CurrencyPair,
    currentPosition: Option[PositionState],
    latestPrice: Option[PriceRange],
    signals: Map[String, List[IndicatorState]],
    lastUpdatedAt: Option[Instant],
    createdAt: Option[Instant]
) derives Codec.AsObject
