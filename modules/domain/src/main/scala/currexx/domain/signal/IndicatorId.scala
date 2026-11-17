package currexx.domain.signal

import currexx.domain.types.StringType
import io.circe.{KeyDecoder, KeyEncoder}
import sttp.tapir.Schema

opaque type IndicatorId = String
object IndicatorId extends StringType[IndicatorId]:
  given KeyEncoder[IndicatorId] = _.value
  given KeyDecoder[IndicatorId] = key => Some(apply(key))
  given Schema[IndicatorId]     = Schema.string

  val Trend                                  = apply("trend")
  val Crossover                              = apply("crossover")
  val MomentumZone                           = apply("momentum-zone")
  val VolatilityRegime                       = apply("volatility-regime")
  val Band                                   = apply("band")
  private val valueIds                       = ValueRole.values.map(role => role -> apply(role.print)).toMap
  private val lineIds                        = ValueRole.values.map(role => role -> apply(s"${role.print}-line")).toMap
  def forValue(role: ValueRole): IndicatorId = valueIds(role)
  def forLine(role: ValueRole): IndicatorId  = lineIds(role)
  val reserved: Set[IndicatorId]             = Set(Trend, Crossover, MomentumZone, VolatilityRegime, Band) ++
    ValueRole.values.flatMap(role => List(forValue(role), forLine(role)))

  def defaultFor(indicator: Indicator): Option[IndicatorId] = indicator match
    case _: Indicator.Composite                                    => None
    case Indicator.Named(_, definition)                            => defaultFor(definition)
    case _: Indicator.TrendChangeDetection                         => Some(Trend)
    case _: Indicator.LinesCrossing                                => Some(Crossover)
    case _: Indicator.ThresholdCrossing                            => Some(MomentumZone)
    case _: Indicator.VolatilityRegimeDetection                    => Some(VolatilityRegime)
    case _: Indicator.KeltnerChannel | _: Indicator.BollingerBands => Some(Band)
    case i: Indicator.ValueTracking                                => Some(forValue(i.role))
    case i: Indicator.PriceLineCrossing                            => Some(forLine(i.role))
