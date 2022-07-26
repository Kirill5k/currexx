package currexx.domain.market

import cats.syntax.either.*
import io.circe.{Decoder, Encoder}
import squants.market.{Currency, defaultMoneyContext}

final case class CurrencyPair(base: Currency, quote: Currency):
  override def toString: String = s"${base.code}${quote.code}"

object CurrencyPair:
  inline def from(strRepr: String): Either[String, CurrencyPair] =
    for
      pair <- Either.cond(
        strRepr.matches("^[A-Z]{3}\\(/)?[A-Z]{3}$"),
        (strRepr.take(3), strRepr.substring(strRepr.length - 3, strRepr.length)),
        s"$strRepr is not valid currency pair representation"
      )
      base  <- Currency(pair._1)(defaultMoneyContext).toEither.leftMap(_.getMessage)
      quote <- Currency(pair._2)(defaultMoneyContext).toEither.leftMap(_.getMessage)
    yield CurrencyPair(base, quote)

  inline given Encoder[CurrencyPair] = Encoder.encodeString.contramap(_.toString)
  inline given Decoder[CurrencyPair] = Decoder.decodeString.emap(CurrencyPair.from(_))
