package currexx.core.market

import currexx.domain.market.{Condition, CurrencyPair, Indicator, PriceRange, TradeOrder}
import currexx.domain.user.UserId
import io.circe.Codec

import java.time.Instant

final case class IndicatorState(condition: Condition, time: Instant) derives Codec.AsObject

final case class MarketState(
    userId: UserId,
    currencyPair: CurrencyPair,
    currentPosition: Option[TradeOrder.Position],
    latestPrice: Option[PriceRange],
    signals: Map[Indicator, List[IndicatorState]],
    lastUpdatedAt: Option[Instant]
) derives Codec.AsObject
