package currexx.domain.market

import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.types.{EnumType, Kinded}
import org.latestbit.circe.adt.codec.*

enum MovingAverage(val kind: String) extends Kinded(kind):
  case Weighted    extends MovingAverage("weighted")
  case Exponential extends MovingAverage("exponential")
  case Simple      extends MovingAverage("simple")
object MovingAverage extends EnumType[MovingAverage](() => MovingAverage.values)

enum CompositeMovingAverage(val kind: String) extends Kinded(kind):
  case Triple  extends CompositeMovingAverage("triple")
  case Hull    extends CompositeMovingAverage("hull")
  case Nyquist extends CompositeMovingAverage("nyquist")
object CompositeMovingAverage extends EnumType[CompositeMovingAverage](() => CompositeMovingAverage.values)

enum ValueSource(val kind: String) extends Kinded(kind):
  case Close extends ValueSource("close")
  case Open  extends ValueSource("open")
  case HL2   extends ValueSource("hl2")
object ValueSource extends EnumType[ValueSource](() => ValueSource.values)

enum ValueTransformation(val kind: String) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Sequenced(sequence: List[ValueTransformation]) extends ValueTransformation("sequenced")
  case Kalman(gain: Double)                           extends ValueTransformation("kalman")
  case WMA(length: Int)                               extends ValueTransformation("wma")
  case SMA(length: Int)                               extends ValueTransformation("sma")
  case EMA(length: Int)                               extends ValueTransformation("ema")
  case HMA(length: Int)                               extends ValueTransformation("hma")
  case NMA(
      length: Int,
      signalLength: Int,
      lambda: Double,
      maCalc: MovingAverage
  )                                                           extends ValueTransformation("nma")
  case STOCH(length: Int, slowKLength: Int, slowDLength: Int) extends ValueTransformation("stoch")

object ValueTransformation:
  def sequenced(sequence: ValueTransformation*): ValueTransformation =
    ValueTransformation.Sequenced(sequence.toList)

  given JsonTaggedAdt.Config[ValueTransformation] = JsonTaggedAdt.Config.Values[ValueTransformation](
    mappings = Map(
      "sequenced" -> JsonTaggedAdt.tagged[ValueTransformation.Sequenced],
      "kalman"    -> JsonTaggedAdt.tagged[ValueTransformation.Kalman],
      "ema"       -> JsonTaggedAdt.tagged[ValueTransformation.EMA],
      "hma"       -> JsonTaggedAdt.tagged[ValueTransformation.HMA],
      "nma"       -> JsonTaggedAdt.tagged[ValueTransformation.NMA],
      "sma"       -> JsonTaggedAdt.tagged[ValueTransformation.SMA],
      "wma"       -> JsonTaggedAdt.tagged[ValueTransformation.WMA],
      "stoch"     -> JsonTaggedAdt.tagged[ValueTransformation.STOCH]
    ),
    strict = true,
    typeFieldName = "kind"
  )

enum Indicator(val kind: String) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case TrendChangeDetection(
      source: ValueSource,
      transformation: ValueTransformation
  ) extends Indicator("trend-change-detection")
  case ThresholdCrossing(
      source: ValueSource,
      transformation: ValueTransformation,
      upperBoundary: BigDecimal,
      lowerBoundary: BigDecimal
  ) extends Indicator("threshold-crossing")

object Indicator:
  given JsonTaggedAdt.Config[Indicator] = JsonTaggedAdt.Config.Values[Indicator](
    mappings = Map(
      "trend-change-detection" -> JsonTaggedAdt.tagged[Indicator.TrendChangeDetection],
      "threshold-crossing"     -> JsonTaggedAdt.tagged[Indicator.ThresholdCrossing]
    ),
    strict = true,
    typeFieldName = "kind"
  )
