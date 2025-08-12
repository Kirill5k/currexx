package currexx.domain.market

import cats.Order
import io.circe.{Decoder, Encoder}

final case class CurrencyPair(base: Currency, quote: Currency):
  override def toString: String = s"${base.code}${quote.code}"

object CurrencyPair:
  inline def from(strRepr: String): Either[String, CurrencyPair] =
    for
      pair <- Either.cond(
        strRepr.matches("^[A-Z]{3}\\/?[A-Z]{3}$"),
        (strRepr.take(3), strRepr.substring(strRepr.length - 3, strRepr.length)),
        s"$strRepr is not valid currency pair representation"
      )
      base  <- Currency.from(pair._1)
      quote <- Currency.from(pair._2)
    yield CurrencyPair(base, quote)

  inline given Encoder[CurrencyPair]  = Encoder.encodeString.contramap(_.toString)
  inline given Decoder[CurrencyPair]  = Decoder.decodeString.emap(CurrencyPair.from(_))
  inline given Order[CurrencyPair]    = Order.from((cp1, cp2) => cp1.toString.compareTo(cp2.toString))
  inline given Ordering[CurrencyPair] = Order[CurrencyPair].toOrdering
