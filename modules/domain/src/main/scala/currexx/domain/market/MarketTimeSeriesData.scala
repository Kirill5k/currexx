package currexx.domain.market

import cats.data.NonEmptyList
import io.circe.{Codec, Decoder, Encoder}

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

enum Interval(val number: Int, val unit: String) {
  case M1  extends Interval(1, "minute")
  case M5  extends Interval(5, "minute")
  case M15 extends Interval(15, "minute")
  case M30 extends Interval(30, "minute")
  case H1  extends Interval(1, "hour")
  case D1  extends Interval(1, "day")

  def toDuration: FiniteDuration = FiniteDuration(number, unit)
}

object Interval {
  given Codec[Interval] = Codec.from(
    Decoder.decodeString.emap(s => scala.util.Try(Interval.valueOf(s)).toEither.left.map(_ => s"Unknown interval $s")),
    Encoder.encodeString.contramap(_.toString)
  )
}

final case class PriceRange(
    open: Double,
    high: Double,
    low: Double,
    close: Double,
    volume: Double,
    time: Instant
) derives Codec.AsObject

final case class MarketTimeSeriesData(
    currencyPair: CurrencyPair,
    interval: Interval,
    prices: NonEmptyList[PriceRange],
    source: String
) {
  def closings: List[Double] = prices.toList.map(_.close)
  def highs: List[Double]    = prices.toList.map(_.high)
  def lows: List[Double]     = prices.toList.map(_.low)
  def volumes: List[Double]  = prices.toList.map(_.volume)
  def latestTime: Instant    = prices.head.time
}
