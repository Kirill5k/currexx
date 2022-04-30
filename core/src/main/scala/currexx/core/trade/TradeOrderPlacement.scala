package currexx.core.trade

import currexx.domain.market.{TradeOrder, PriceRange}
import currexx.domain.user.UserId

import java.time.Instant

final case class TradeOrderPlacement(
    userId: UserId,
    order: TradeOrder,
    currentPrice: PriceRange,
    time: Instant
)
