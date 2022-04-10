package currexx.domain.market

import cats.syntax.either.*
import io.circe.{Decoder, Encoder}
import squants.market.{Currency, defaultMoneyContext}

final case class CurrencyPair(base: Currency, quote: Currency)
object CurrencyPair:
  inline given Encoder[CurrencyPair] = Encoder.encodeString.contramap(cp => s"${cp.base.code}/${cp.quote.code}")
  inline given Decoder[CurrencyPair] = Decoder.decodeString.emap { cp =>
    for
      pair  <- Either.cond(cp.matches("^[A-Z]{3}\\/[A-Z]{3}$"), cp.split("/"), s"$cp is not valid currency pair representation")
      base  <- Currency(pair.head)(defaultMoneyContext).toEither.leftMap(_.getMessage)
      quote <- Currency(pair.last)(defaultMoneyContext).toEither.leftMap(_.getMessage)
    yield CurrencyPair(base, quote)
  }

