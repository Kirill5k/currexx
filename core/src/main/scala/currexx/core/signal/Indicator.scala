package currexx.core.signal

import io.circe.{Codec, Decoder, DecodingFailure, Encoder}
import io.circe.syntax.*

import scala.util.Try

enum Direction:
  case Up, Down
object Direction:
  inline given Encoder[Direction] = Encoder.encodeString.contramap(_.toString.toLowerCase)
  inline given Decoder[Direction] = Decoder.decodeString.emapTry(s => Try(Direction.valueOf(s.capitalize)))

sealed trait Indicator
object Indicator:
  final case class MACD(direction: Direction, value: BigDecimal) extends Indicator derives Codec.AsObject

  inline given Decoder[Indicator] = Decoder.instance { c =>
    c.downField("kind").as[String].flatMap {
      case "macd" => c.as[Indicator.MACD]
      case kind   => Left(DecodingFailure(s"unexpected indicator kind $kind", Nil))
    }
  }
  inline given Encoder[Indicator] = Encoder.instance {
    case macd: Indicator.MACD => macd.asJson.deepMerge(Map("kind" -> "macd").asJson)
  }
