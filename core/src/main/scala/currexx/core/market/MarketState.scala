package currexx.core.market

import currexx.domain.market.{CurrencyPair, MarketOrder, PriceRange}
import currexx.domain.user.UserId
import io.circe.Codec

import java.time.Instant

final case class MarketState(
    userId: UserId,
    currencyPair: CurrencyPair,
    currentPosition: Option[MarketOrder.Position],
    latestPrice: Option[PriceRange],
    lastUpdatedAt: Option[Instant]
) derives Codec.AsObject
