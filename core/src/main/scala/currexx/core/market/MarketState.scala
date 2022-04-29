package currexx.core.market

import currexx.domain.market.{Condition, CurrencyPair, Indicator, MarketOrder, PriceRange}
import currexx.domain.user.UserId
import io.circe.Codec

import java.time.Instant

final case class IndicatorState(condition: Condition, time: Instant) derives Codec.AsObject

final case class MarketState(
    userId: UserId,
    currencyPair: CurrencyPair,
    currentPosition: Option[MarketOrder.Position],
    latestPrice: Option[PriceRange],
    signals: Map[Indicator, List[IndicatorState]],
    lastUpdatedAt: Option[Instant]
) derives Codec.AsObject
