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
  case Momentum, Volatility

enum ValueTransformation(val kind: String) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Sequenced(sequence: List[ValueTransformation])                             extends ValueTransformation("sequenced")
  case Kalman(gain: Double)                                                       extends ValueTransformation("kalman")
  case KalmanVelocity(gain: Double)                                               extends ValueTransformation("kalman-velocity")
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

object IndicatorKind extends EnumType[IndicatorKind](() => IndicatorKind.values)
enum IndicatorKind:
  case VolatilityRegimeDetection, TrendChangeDetection, ThresholdCrossing, LinesCrossing, KeltnerChannel, Composite, ValueTracking

enum Indicator(val kind: IndicatorKind) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Composite(
      indicators: NonEmptyList[Indicator]
  ) extends Indicator(IndicatorKind.Composite)
  case TrendChangeDetection(
      source: ValueSource,
      transformation: ValueTransformation
  ) extends Indicator(IndicatorKind.TrendChangeDetection)
  case ThresholdCrossing(
      source: ValueSource,
      transformation: ValueTransformation,
      upperBoundary: Double,
      lowerBoundary: Double
  ) extends Indicator(IndicatorKind.ThresholdCrossing)
  case LinesCrossing(
      source: ValueSource,
      line1Transformation: ValueTransformation, // SLOW
      line2Transformation: ValueTransformation  // FAST
  ) extends Indicator(IndicatorKind.LinesCrossing)
  case KeltnerChannel(
      source: ValueSource,
      line1Transformation: ValueTransformation, // SLOW
      line2Transformation: ValueTransformation, // FAST
      atrLength: Int,
      atrMultiplier: Double
  ) extends Indicator(IndicatorKind.KeltnerChannel)
  case VolatilityRegimeDetection(
      atrLength: Int,
      smoothingType: ValueTransformation,
      smoothingLength: Int
  ) extends Indicator(IndicatorKind.VolatilityRegimeDetection)
  case ValueTracking(
      role: ValueRole,
      source: ValueSource,
      transformation: ValueTransformation
  ) extends Indicator(IndicatorKind.ValueTracking)

object Indicator:
  given JsonTaggedAdt.Config[Indicator] = JsonTaggedAdt.Config.Values[Indicator](
    mappings = Map(
      IndicatorKind.Composite.print                 -> JsonTaggedAdt.tagged[Indicator.Composite],
      IndicatorKind.VolatilityRegimeDetection.print -> JsonTaggedAdt.tagged[Indicator.VolatilityRegimeDetection],
      IndicatorKind.TrendChangeDetection.print      -> JsonTaggedAdt.tagged[Indicator.TrendChangeDetection],
      IndicatorKind.ThresholdCrossing.print         -> JsonTaggedAdt.tagged[Indicator.ThresholdCrossing],
      IndicatorKind.LinesCrossing.print             -> JsonTaggedAdt.tagged[Indicator.LinesCrossing],
      IndicatorKind.KeltnerChannel.print            -> JsonTaggedAdt.tagged[Indicator.KeltnerChannel],
      IndicatorKind.ValueTracking.print             -> JsonTaggedAdt.tagged[Indicator.ValueTracking]
    ),
    strict = true,
    typeFieldName = "kind"
  )
