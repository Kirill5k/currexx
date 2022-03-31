package currex.core.common

import cats.syntax.either.*
import io.circe.{Decoder, Encoder, Json, JsonObject}
import squants.market.{Currency, Money, defaultMoneyContext}

import scala.util.Try

object json extends JsonCodecs

transparent trait JsonCodecs {
  inline given Decoder[Currency] = Decoder[JsonObject].emap { json =>
    for
      code     <- json("code").flatMap(_.asString).toRight("Missing currency code")
      currency <- Currency(code)(defaultMoneyContext).toEither.leftMap(_.getMessage)
    yield currency
  }

  inline given Encoder[Currency] = Encoder[JsonObject].contramap { c =>
    JsonObject(
      "code"   -> Json.fromString(c.code),
      "symbol" -> Json.fromString(c.symbol)
    )
  }

  inline given monDec(using d: Decoder[Currency]): Decoder[Money] = Decoder[JsonObject].emap { json =>
    for
      rawValue    <- json("value").flatMap(_.asNumber).toRight("Missing the actual amount")
      rawCurrency <- json("currency").toRight("Missing currency")
      currency    <- d.decodeJson(rawCurrency).leftMap(_.message)
      value       <- Try(rawValue.toDouble).map(roundUp).toEither.leftMap(_.getMessage)
    yield Money(value, currency)
  }

  inline given monEnc(using e: Encoder[Currency]): Encoder[Money] = Encoder[JsonObject].contramap { m =>
    JsonObject(
      "value"    -> Json.fromBigDecimal(roundUp(m.amount)),
      "currency" -> e(m.currency)
    )
  }

  private def roundUp(value: BigDecimal): BigDecimal = value.setScale(2, BigDecimal.RoundingMode.HALF_UP)
  private def roundUp(value: Double): BigDecimal     = roundUp(BigDecimal(value))
}
