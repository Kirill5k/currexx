package currexx.domain.signal

import cats.data.NonEmptyList
import currexx.domain.types.EnumType
import io.circe.{Decoder, Encoder}
import org.latestbit.circe.adt.codec.*

object MovingAverage extends EnumType[MovingAverage](() => MovingAverage.values)
enum MovingAverage:
  case Weighted, Exponential, Simple, Hull

object CompositeMovingAverage extends EnumType[CompositeMovingAverage](() => CompositeMovingAverage.values)
enum CompositeMovingAverage:
  case Triple, Nyquist, Jurik

object ValueSource extends EnumType[ValueSource](() => ValueSource.values, EnumType.printLowerCase(_))
enum ValueSource:
  case Close, Open, HL2, HLC3

object ValueRole extends EnumType[ValueRole](() => ValueRole.values)
enum ValueRole:
  case Momentum, Volatility, Velocity

enum ValueTransformation(val kind: String) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Sequenced(sequence: List[ValueTransformation])                             extends ValueTransformation("sequenced")
  case Kalman(gain: Double, measurementNoise: Double)                             extends ValueTransformation("kalman")
  case KalmanVelocity(gain: Double, measurementNoise: Double)                     extends ValueTransformation("kalman-velocity")
  case RSX(length: Int)                                                           extends ValueTransformation("rsx")
  case JRSX(length: Int)                                                          extends ValueTransformation("jrsx")
  case WMA(length: Int)                                                           extends ValueTransformation("wma")
  case SMA(length: Int)                                                           extends ValueTransformation("sma")
  case EMA(length: Int)                                                           extends ValueTransformation("ema")
  case HMA(length: Int)                                                           extends ValueTransformation("hma")
  case NMA(length: Int, signalLength: Int, lambda: Double, maCalc: MovingAverage) extends ValueTransformation("nma")
  case JMA(length: Int, phase: Int, power: Int)                                   extends ValueTransformation("jma")
  case STOCH(length: Int)                                                         extends ValueTransformation("stoch")

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

enum Indicator(val kind: String) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  //TODO: add logic parameter
  case Composite(
      indicators: NonEmptyList[Indicator]
  ) extends Indicator("composite")
  case TrendChangeDetection(
      source: ValueSource,
      transformation: ValueTransformation
  ) extends Indicator("trend-change-detection")
  case ThresholdCrossing(
      source: ValueSource,
      transformation: ValueTransformation,
      upperBoundary: Double,
      lowerBoundary: Double
  ) extends Indicator("threshold-crossing")
  case LinesCrossing(
      source: ValueSource,
      line1Transformation: ValueTransformation, // SLOW
      line2Transformation: ValueTransformation  // FAST
  ) extends Indicator("lines-crossing")
  case KeltnerChannel(
      source: ValueSource,
      line1Transformation: ValueTransformation, // SLOW
      line2Transformation: ValueTransformation, // FAST
      atrLength: Int,
      atrMultiplier: Double
  ) extends Indicator("keltner-channel")
  case VolatilityRegimeDetection(
      atrLength: Int,
      smoothingType: ValueTransformation,
      smoothingLength: Int
  ) extends Indicator("volatility-regime-detection")
  case ValueTracking(
      role: ValueRole,
      source: ValueSource,
      transformation: ValueTransformation
  ) extends Indicator("value-tracking")

object Indicator:
  given JsonTaggedAdt.Config[Indicator] = JsonTaggedAdt.Config.Values[Indicator](
    mappings = Map(
      "composite"                   -> JsonTaggedAdt.tagged[Indicator.Composite],
      "volatility-regime-detection" -> JsonTaggedAdt.tagged[Indicator.VolatilityRegimeDetection],
      "trend-change-detection"       -> JsonTaggedAdt.tagged[Indicator.TrendChangeDetection],
      "threshold-crossing"          -> JsonTaggedAdt.tagged[Indicator.ThresholdCrossing],
      "lines-crossing"              -> JsonTaggedAdt.tagged[Indicator.LinesCrossing],
      "keltner-channel"             -> JsonTaggedAdt.tagged[Indicator.KeltnerChannel],
      "value-tracking"              -> JsonTaggedAdt.tagged[Indicator.ValueTracking]
    ),
    strict = true,
    typeFieldName = "kind"
  )
