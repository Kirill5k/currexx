package currexx.domain.market

import io.circe.{Decoder, Encoder}

import scala.util.Try

enum Trend:
  case Upward, Downward, Stale

object Trend:
  inline given Encoder[Trend] = Encoder.encodeString.contramap(_.toString.toLowerCase)
  inline given Decoder[Trend] = Decoder.decodeString.emapTry(s => Try(Trend.valueOf(s.capitalize)))
