package currexx.domain.market

import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*

sealed trait Condition(val kind: String)
object Condition {
  final case class CrossingUp(value: BigDecimal)   extends Condition("crossing-up") derives Codec.AsObject
  final case class CrossingDown(value: BigDecimal) extends Condition("crossing-down") derives Codec.AsObject

  private val discriminatorField: String               = "kind"
  private def discriminatorJson(cond: Condition): Json = Map(discriminatorField -> cond.kind).asJson

  inline given Decoder[Condition] = Decoder.instance { c =>
    c.downField(discriminatorField).as[String].flatMap {
      case "crossing-up"   => c.as[CrossingUp]
      case "crossing-down" => c.as[CrossingDown]
      case kind            => Left(DecodingFailure(s"Unexpected condition kind $kind", List(CursorOp.Field(discriminatorField))))
    }
  }
  inline given Encoder[Condition] = Encoder.instance {
    case crossUp: CrossingUp     => crossUp.asJson.deepMerge(discriminatorJson(crossUp))
    case crossDown: CrossingDown => crossDown.asJson.deepMerge(discriminatorJson(crossDown))
  }
}
