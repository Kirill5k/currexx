package currexx.domain.market

import io.circe.{Decoder, Encoder}

import scala.util.Try

enum Trend:
  case Upward, Downward, Stale

object Trend:
  inline given Encoder[Trend] = Encoder.encodeString.contramap(_.toString.toLowerCase)
  inline given Decoder[Trend] = Decoder.decodeString.emap { t =>
    Try(Trend.valueOf(t.capitalize)).toOption
      .toRight(s"$t is not valid trend value. Accepted value: ${Trend.values.map(_.toString.toLowerCase).mkString(", ")}")
  }
