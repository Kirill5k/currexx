package currexx.domain.market

import io.circe.{Decoder, Encoder}
import sttp.tapir.{Schema, Validator}

opaque type CandleCount = Int

object CandleCount {
  val Default: CandleCount = 100

  // Reserve one of the providers' 5,000 records for the current incomplete candle.
  def from(count: Int): Either[String, CandleCount] =
    Either.cond(count >= 1 && count <= 4999, count, "candleCount must be between 1 and 4999")

  def fromUnsafe(count: Int): CandleCount =
    from(count).fold(err => throw new IllegalArgumentException(err), identity)

  extension (count: CandleCount) def value: Int = count

  given Decoder[CandleCount] = Decoder.decodeJson.emap { json =>
    json.asNumber.flatMap(_.toInt).toRight("candleCount must be an integer").flatMap(from)
  }
  given Encoder[CandleCount] = Encoder.encodeInt.contramap(_.value)
  given Schema[CandleCount] = Schema.schemaForInt
    .validate(Validator.min(1))
    .validate(Validator.max(4999))
    .map(count => from(count).toOption)(_.value)
    .description("Maximum number of candles used for signal calculation")
}
