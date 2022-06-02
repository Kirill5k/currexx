package currexx.domain.market

import cats.data.NonEmptyList
import io.circe.Codec
import org.latestbit.circe.adt.codec.*

import java.time.Instant
import scala.util.Try

enum Interval derives JsonTaggedAdt.PureEncoder, JsonTaggedAdt.PureDecoder:
  case M1, M5, M15, M30, H1, D1

final case class PriceRange(
    open: BigDecimal,
    high: BigDecimal,
    low: BigDecimal,
    close: BigDecimal,
    volume: BigDecimal,
    time: Instant
) derives Codec.AsObject

final case class MarketTimeSeriesData(
    currencyPair: CurrencyPair,
    interval: Interval,
    prices: NonEmptyList[PriceRange]
)
