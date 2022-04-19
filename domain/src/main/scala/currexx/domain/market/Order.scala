package currexx.domain.market

import io.circe.{Codec, Decoder, Encoder}

import scala.util.Try

enum Position:
  case Buy, Sell, Close

object Position:
  inline given Encoder[Position] = Encoder.encodeString.contramap(_.toString.toUpperCase)
  inline given Decoder[Position] = Decoder.decodeString.emap { p =>
    Try(Position.valueOf(p.toLowerCase.capitalize)).toOption
      .toRight(s"$p is not valid position value. Accepted value: ${Position.values.map(_.toString.toUpperCase).mkString(", ")}")
  }

final case class Order(
    currencyPair: CurrencyPair,
    position: Position
) derives Codec.AsObject
