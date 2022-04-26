package currexx.core.market

import currexx.domain.market.{CurrencyPair, MarketOrder}
import currexx.domain.user.UserId

import java.time.Instant

final case class CurrencyState(
    currentPosition: Option[MarketOrder.Position],
    lastUpdatedAt: Option[Instant]
)

final case class MarketState(
    userId: UserId,
    currencies: Map[CurrencyPair, CurrencyPair]
)
