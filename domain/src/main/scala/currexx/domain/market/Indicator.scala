package currexx.domain.market

import io.circe.{Decoder, Encoder}

enum Indicator(val kind: String):
  case MACD extends Indicator("macd")
  case RSI  extends Indicator("rsi")

object Indicator:
  def from(kind: String): Either[String, Indicator] =
    Indicator.values.find(_.kind == kind).toRight(s"Unrecognized indicator $kind")
  inline given Decoder[Indicator] = Decoder[String].emap(Indicator.from)
  inline given Encoder[Indicator] = Encoder[String].contramap(_.kind)
