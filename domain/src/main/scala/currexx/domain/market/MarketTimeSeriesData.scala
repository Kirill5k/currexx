package currexx.domain.market

import cats.data.NonEmptyList
import io.circe.{Codec, Decoder, Encoder}

import java.time.Instant
import scala.util.Try

enum Interval:
  case M1, M5, M15, M30, H1, D1

object Interval:
  inline given Encoder[Interval] = Encoder.encodeString.contramap(_.toString.toUpperCase)
  inline given Decoder[Interval] = Decoder.decodeString.emap { i =>
    Try(Interval.valueOf(i.toUpperCase)).toOption
      .toRight(s"$i is not valid interval value. Accepted value: ${Interval.values.map(_.toString).mkString(", ")}")
  }

final case class PriceRange(
    open: BigDecimal,
    high: BigDecimal,
    low: BigDecimal,
    close: BigDecimal,
    volume: BigDecimal,
    time: Instant
)

final case class MarketTimeSeriesData(
    currencyPair: CurrencyPair,
    interval: Interval,
    prices: NonEmptyList[PriceRange]
)
