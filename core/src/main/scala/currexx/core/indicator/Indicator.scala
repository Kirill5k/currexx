package currexx.core.indicator

import cats.syntax.either.*
import currexx.core.auth.user.UserId
import currexx.core.common.validations.CurrencyPairString
import io.circe.{Codec, Decoder, Encoder}
import squants.market.{Currency, defaultMoneyContext}

import java.time.Instant

enum Direction:
  case Up, Down

enum Indicator:
  case MACD(direction: Direction, value: BigDecimal)

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

final case class Signal(
    userId: UserId,
    currencyPair: CurrencyPair,
    indicator: Indicator,
    time: Instant
)
