package currexx.domain.monitor

import io.circe.{Decoder, Encoder}

final case class Limits(
    min: Option[BigDecimal],
    max: Option[BigDecimal],
    cumulativeMin: Option[BigDecimal],
    cumulativeMax: Option[BigDecimal],
    trailing: Boolean
) derives Encoder.AsObject

object Limits {
  inline given Decoder[Limits] = Decoder.decodeJsonObject.emap { json =>
    val min           = json("min").flatMap(_.asNumber).flatMap(_.toBigDecimal)
    val max           = json("max").flatMap(_.asNumber).flatMap(_.toBigDecimal)
    val cumulativeMin = json("cumulativeMin").flatMap(_.asNumber).flatMap(_.toBigDecimal)
    val cumulativeMax = json("cumulativeMax").flatMap(_.asNumber).flatMap(_.toBigDecimal)
    val trailing      = json("trailing").flatMap(_.asBoolean).getOrElse(false)
    Either.cond(
      min.isDefined || max.isDefined || cumulativeMin.isDefined || cumulativeMax.isDefined,
      Limits(min, max, cumulativeMin, cumulativeMax, trailing),
      "Limits must have at least one of the fields defined"
    )
  }
}
