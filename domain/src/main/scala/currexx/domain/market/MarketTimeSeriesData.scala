package currexx.domain.market

import java.time.Instant

enum Interval:
  case M1, M5, M15, M30, H1, D1

final case class PriceRange(
    open: BigDecimal,
    high: BigDecimal,
    low: BigDecimal,
    close: BigDecimal,
    time: Instant
)

final case class MarketTimeSeriesData(
    currencyPair: CurrencyPair,
    interval: Interval,
    prices: List[PriceRange]
)
