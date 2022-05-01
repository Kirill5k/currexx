package currexx.core.trade

import currexx.clients.broker.BrokerParameters
import currexx.domain.market.{PriceRange, TradeOrder, CurrencyPair}
import currexx.domain.user.UserId

import java.time.Instant

final case class TradeOrderPlacement(
    userId: UserId,
    currencyPair: CurrencyPair,
    order: TradeOrder,
    broker: BrokerParameters,
    currentPrice: PriceRange,
    time: Instant
)