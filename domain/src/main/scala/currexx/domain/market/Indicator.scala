package currexx.domain.market

import cats.syntax.functor.*
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.types.EnumType
import io.circe.{Decoder, Encoder}
import io.circe.syntax.*
import org.latestbit.circe.adt.codec.*

object MovingAverage extends EnumType[MovingAverage](() => MovingAverage.values)
enum MovingAverage:
  case Weighted, Exponential, Simple, Hull

object CompositeMovingAverage extends EnumType[CompositeMovingAverage](() => CompositeMovingAverage.values)
enum CompositeMovingAverage:
  case Triple, Nyquist, Jurik

object ValueSource extends EnumType[ValueSource](() => ValueSource.values, EnumType.printLowerCase(_))
enum ValueSource:
  case Close, Open, HL2

sealed trait ValueTransformation

object ValueTransformation {
  enum SingleOutput(val kind: String) extends ValueTransformation derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
    case Sequenced(sequence: List[SingleOutput])                                    extends SingleOutput("sequenced")
    case Kalman(gain: Double)                                                       extends SingleOutput("kalman")
    case RSX(length: Int)                                                           extends SingleOutput("rsx")
    case WMA(length: Int)                                                           extends SingleOutput("wma")
    case SMA(length: Int)                                                           extends SingleOutput("sma")
    case EMA(length: Int)                                                           extends SingleOutput("ema")
    case HMA(length: Int)                                                           extends SingleOutput("hma")
    case NMA(length: Int, signalLength: Int, lambda: Double, maCalc: MovingAverage) extends SingleOutput("nma")
    case JMA(length: Int, phase: Int, power: Int)                                   extends SingleOutput("jma")

  object SingleOutput:
    def sequenced(so: SingleOutput, soSequence: SingleOutput*): SingleOutput = SingleOutput.Sequenced(so :: soSequence.toList)

    given JsonTaggedAdt.Config[SingleOutput] = JsonTaggedAdt.Config.Values[SingleOutput](
      mappings = Map(
        "sequenced" -> JsonTaggedAdt.tagged[SingleOutput.Sequenced],
        "kalman"    -> JsonTaggedAdt.tagged[SingleOutput.Kalman],
        "rsx"       -> JsonTaggedAdt.tagged[SingleOutput.RSX],
        "ema"       -> JsonTaggedAdt.tagged[SingleOutput.EMA],
        "hma"       -> JsonTaggedAdt.tagged[SingleOutput.HMA],
        "nma"       -> JsonTaggedAdt.tagged[SingleOutput.NMA],
        "sma"       -> JsonTaggedAdt.tagged[SingleOutput.SMA],
        "wma"       -> JsonTaggedAdt.tagged[SingleOutput.WMA],
        "jma"       -> JsonTaggedAdt.tagged[SingleOutput.JMA]
      ),
      strict = true,
      typeFieldName = "kind"
    )

  enum DoubleOutput(val kind: String) extends ValueTransformation derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
    case STOCH(length: Int, slowKLength: Int, slowDLength: Int) extends DoubleOutput("stoch")

  object DoubleOutput:
    given JsonTaggedAdt.Config[DoubleOutput] = JsonTaggedAdt.Config.Values[DoubleOutput](
      mappings = Map(
        "stoch" -> JsonTaggedAdt.tagged[DoubleOutput.STOCH]
      ),
      strict = true,
      typeFieldName = "kind"
    )

  given Encoder[ValueTransformation] = Encoder.instance {
    case vt: SingleOutput => vt.asJson
    case vt: DoubleOutput => vt.asJson
  }

  given Decoder[ValueTransformation] = List[Decoder[ValueTransformation]](
    Decoder[SingleOutput].widen,
    Decoder[DoubleOutput].widen
  ).reduceLeft(_ or _)
}

object IndicatorKind extends EnumType[IndicatorKind](() => IndicatorKind.values)
enum IndicatorKind:
  case TrendChangeDetection, ThresholdCrossing, LinesCrossing

enum Indicator(val kind: IndicatorKind) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case TrendChangeDetection(
      source: ValueSource,
      transformation: ValueTransformation.SingleOutput
  ) extends Indicator(IndicatorKind.TrendChangeDetection)
  case ThresholdCrossing(
      source: ValueSource,
      transformation: ValueTransformation,
      upperBoundary: Double,
      lowerBoundary: Double
  ) extends Indicator(IndicatorKind.ThresholdCrossing)
  case LinesCrossing(
      source: ValueSource,
      line1Transformation: ValueTransformation.SingleOutput,
      line2Transformation: ValueTransformation.SingleOutput
  ) extends Indicator(IndicatorKind.LinesCrossing)

object Indicator:
  given JsonTaggedAdt.Config[Indicator] = JsonTaggedAdt.Config.Values[Indicator](
    mappings = Map(
      IndicatorKind.TrendChangeDetection.print -> JsonTaggedAdt.tagged[Indicator.TrendChangeDetection],
      IndicatorKind.ThresholdCrossing.print    -> JsonTaggedAdt.tagged[Indicator.ThresholdCrossing],
      IndicatorKind.LinesCrossing.print        -> JsonTaggedAdt.tagged[Indicator.LinesCrossing]
    ),
    strict = true,
    typeFieldName = "kind"
  )
