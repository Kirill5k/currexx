package currexx.domain

import cats.syntax.either.*
import io.circe.{Decoder, Encoder, Json, JsonObject}
import squants.market.{Currency, defaultMoneyContext, Money}

import scala.util.Try

object json extends JsonCodecs

transparent trait JsonCodecs {
  inline given Decoder[Currency] = Decoder.decodeString.emap(c => Currency(c)(defaultMoneyContext).toEither.leftMap(_.getMessage))
  inline given Encoder[Currency] = Encoder.encodeString.contramap(_.code)

  inline given monDec(using d: Decoder[Currency]): Decoder[Money] = Decoder[JsonObject].emap { json =>
    for
      rawValue    <- json("amount").flatMap(_.asNumber).toRight("Missing the actual amount")
      rawCurrency <- json("currency").toRight("Missing currency code")
      currency    <- d.decodeJson(rawCurrency).leftMap(_.message)
      value       <- Try(rawValue.toDouble).map(roundUp).toEither.leftMap(_.getMessage)
    yield Money(value, currency)
  }

  inline given monEnc(using e: Encoder[Currency]): Encoder[Money] = Encoder[JsonObject].contramap { m =>
    JsonObject(
      "amount"   -> Json.fromBigDecimal(roundUp(m.amount)),
      "currency" -> e(m.currency)
    )
  }

  private def roundUp(value: BigDecimal): BigDecimal = value.setScale(2, BigDecimal.RoundingMode.HALF_UP)
  private def roundUp(value: Double): BigDecimal     = roundUp(BigDecimal(value))
}
