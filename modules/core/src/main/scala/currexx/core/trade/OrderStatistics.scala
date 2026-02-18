package currexx.core.trade

import currexx.domain.market.CurrencyPair
import io.circe.Codec

import java.time.Instant

final case class OrderStatistics(
    totalOrders: Int,
    successfulOrders: Int,
    pendingOrders: Int,
    cancelledOrders: Int,
    enterOrders: EnterOrderStats,
    exitOrders: ExitOrderStats,
    currencyBreakdown: List[CurrencyStatistics],
    timeRange: Option[TimeRange]
) derives Codec.AsObject

final case class EnterOrderStats(
    total: Int,
    buyCount: Int,
    sellCount: Int,
    totalVolume: BigDecimal,
    averageVolume: Option[BigDecimal]
) derives Codec.AsObject

final case class ExitOrderStats(
    total: Int
) derives Codec.AsObject

final case class CurrencyStatistics(
    currencyPair: CurrencyPair,
    totalOrders: Int,
    successfulOrders: Int,
    pendingOrders: Int,
    cancelledOrders: Int,
    enterOrders: Int,
    exitOrders: Int,
    buyOrders: Int,
    sellOrders: Int,
    totalVolume: BigDecimal
) derives Codec.AsObject

final case class TimeRange(
    from: Instant,
    to: Instant
) derives Codec.AsObject
