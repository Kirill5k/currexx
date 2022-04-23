package currexx.domain.market

import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*

sealed trait Condition(val kind: String)
object Condition {
  case object CrossingUp                                                    extends Condition("crossing-up")
  case object CrossingDown                                                  extends Condition("crossing-down")
  final case class AboveThreshold(threshold: BigDecimal, value: BigDecimal) extends Condition("above-threshold") derives Codec.AsObject
  final case class BelowThreshold(threshold: BigDecimal, value: BigDecimal) extends Condition("below-threshold") derives Codec.AsObject

  private val discriminatorField: String               = "kind"
  private def discriminatorJson(cond: Condition): Json = Map(discriminatorField -> cond.kind).asJson

  inline given Decoder[Condition] = Decoder.instance { c =>
    c.downField(discriminatorField).as[String].flatMap {
      case "crossing-up"     => Right(CrossingUp)
      case "crossing-down"   => Right(CrossingDown)
      case "above-threshold" => c.as[AboveThreshold]
      case "below-threshold" => c.as[BelowThreshold]
      case kind              => Left(DecodingFailure(s"Unexpected condition kind $kind", List(CursorOp.Field(discriminatorField))))
    }
  }
  inline given Encoder[Condition] = Encoder.instance {
    case crossUp @ CrossingUp           => discriminatorJson(crossUp)
    case crossDown @ CrossingDown       => discriminatorJson(crossDown)
    case aboveThreshold: AboveThreshold => aboveThreshold.asJson.deepMerge(discriminatorJson(aboveThreshold))
    case belowThreshold: BelowThreshold => belowThreshold.asJson.deepMerge(discriminatorJson(belowThreshold))
  }

  def lineCrossing(line1Curr: BigDecimal, line2Curr: BigDecimal, line1Prev: BigDecimal, line2Prev: BigDecimal): Option[Condition] =
    (line1Curr, line2Curr, line1Prev, line2Prev) match
      case (l1c, l2c, l1p, l2p) if l1c > l2c && l1p < l2p => Some(Condition.CrossingUp)
      case (l1c, l2c, l1p, l2p) if l1c < l2c && l1p > l2p => Some(Condition.CrossingDown)
      case _                                              => None
}
