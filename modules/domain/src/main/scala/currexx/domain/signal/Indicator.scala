package currexx.domain.signal

import cats.data.NonEmptyList
import currexx.domain.types.EnumType
import io.circe.{Decoder, Encoder}
import org.latestbit.circe.adt.codec.*

object MovingAverage extends EnumType[MovingAverage](() => MovingAverage.values)
enum MovingAverage:
  case Weighted, Exponential, Simple, Hull

object ValueSource extends EnumType[ValueSource](() => ValueSource.values, EnumType.printLowerCase(_))
enum ValueSource:
  case Close, Open, HL2, HLC3

object ValueRole extends EnumType[ValueRole](() => ValueRole.values)
enum ValueRole:
  case Momentum, Volatility, Velocity, ChannelMiddleBand

enum ValueTransformation derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Sequenced(sequence: List[ValueTransformation])                             extends ValueTransformation
  case Kalman(gain: Double, measurementNoise: Double)                             extends ValueTransformation
  case KalmanVelocity(gain: Double, measurementNoise: Double)                     extends ValueTransformation
  case RSX(length: Int)                                                           extends ValueTransformation
  case JRSX(length: Int)                                                          extends ValueTransformation
  case WMA(length: Int)                                                           extends ValueTransformation
  case SMA(length: Int)                                                           extends ValueTransformation
  case EMA(length: Int)                                                           extends ValueTransformation
  case HMA(length: Int)                                                           extends ValueTransformation
  case NMA(length: Int, signalLength: Int, lambda: Double, maCalc: MovingAverage) extends ValueTransformation
  case JMA(length: Int, phase: Int, power: Int)                                   extends ValueTransformation
  case STOCH(length: Int)                                                         extends ValueTransformation

object ValueTransformation {
  def sequenced(vt: ValueTransformation, vtSequence: ValueTransformation*): ValueTransformation =
    ValueTransformation.Sequenced(vt :: vtSequence.toList)

  given JsonTaggedAdt.Config[ValueTransformation] = JsonTaggedAdt.Config.Values[ValueTransformation](
    mappings = Map(
      "sequenced"       -> JsonTaggedAdt.tagged[ValueTransformation.Sequenced],
      "kalman"          -> JsonTaggedAdt.tagged[ValueTransformation.Kalman],
      "kalman-velocity" -> JsonTaggedAdt.tagged[ValueTransformation.KalmanVelocity],
      "rsx"             -> JsonTaggedAdt.tagged[ValueTransformation.RSX],
      "jrsx"            -> JsonTaggedAdt.tagged[ValueTransformation.JRSX],
      "stoch"           -> JsonTaggedAdt.tagged[ValueTransformation.STOCH],
      "ema"             -> JsonTaggedAdt.tagged[ValueTransformation.EMA],
      "hma"             -> JsonTaggedAdt.tagged[ValueTransformation.HMA],
      "nma"             -> JsonTaggedAdt.tagged[ValueTransformation.NMA],
      "sma"             -> JsonTaggedAdt.tagged[ValueTransformation.SMA],
      "wma"             -> JsonTaggedAdt.tagged[ValueTransformation.WMA],
      "jma"             -> JsonTaggedAdt.tagged[ValueTransformation.JMA]
    ),
    strict = true,
    typeFieldName = "kind"
  )
}

object CombinationLogic extends EnumType[CombinationLogic](() => CombinationLogic.values)
enum CombinationLogic:
  case All, Any

enum Indicator derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Composite(
      indicators: NonEmptyList[Indicator],
      combinator: CombinationLogic
  )
  case TrendChangeDetection(
      source: ValueSource,
      transformation: ValueTransformation
  )
  case ThresholdCrossing(
      source: ValueSource,
      transformation: ValueTransformation,
      upperBoundary: Double,
      lowerBoundary: Double
  )
  case LinesCrossing(
      source: ValueSource,
      line1Transformation: ValueTransformation, // SLOW
      line2Transformation: ValueTransformation  // FAST
  )
  case KeltnerChannel(
      source: ValueSource,
      middleBand: ValueTransformation,
      atrLength: Int,
      atrMultiplier: Double
  )
  case VolatilityRegimeDetection(
      atrLength: Int,
      smoothingType: ValueTransformation,
      smoothingLength: Int
  )
  case ValueTracking(
      role: ValueRole,
      source: ValueSource,
      transformation: ValueTransformation
  )
  case PriceLineCrossing(
      source: ValueSource,
      role: ValueRole,
      transformation: ValueTransformation
  )

object Indicator:
  given JsonTaggedAdt.Config[Indicator] = JsonTaggedAdt.Config.Values[Indicator](
    mappings = Map(
      "composite"                   -> JsonTaggedAdt.tagged[Indicator.Composite],
      "volatility-regime-detection" -> JsonTaggedAdt.tagged[Indicator.VolatilityRegimeDetection],
      "trend-change-detection"      -> JsonTaggedAdt.tagged[Indicator.TrendChangeDetection],
      "threshold-crossing"          -> JsonTaggedAdt.tagged[Indicator.ThresholdCrossing],
      "lines-crossing"              -> JsonTaggedAdt.tagged[Indicator.LinesCrossing],
      "keltner-channel"             -> JsonTaggedAdt.tagged[Indicator.KeltnerChannel],
      "value-tracking"              -> JsonTaggedAdt.tagged[Indicator.ValueTracking],
      "price-line-crossing"         -> JsonTaggedAdt.tagged[Indicator.PriceLineCrossing]
    ),
    strict = true,
    typeFieldName = "kind"
  )

  def compositeAllOf(indicator: Indicator, indicators: Indicator*): Indicator =
    Indicator.Composite(NonEmptyList.of(indicator, indicators*), CombinationLogic.All)

  def compositeAnyOf(indicator: Indicator, indicators: Indicator*): Indicator =
    Indicator.Composite(NonEmptyList.of(indicator, indicators*), CombinationLogic.Any)
