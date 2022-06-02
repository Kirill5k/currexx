package currexx.domain.market.v2

import org.latestbit.circe.adt.codec.*

enum MovingAverage(val kind: String) derives JsonTaggedAdt.PureEncoderWithConfig, JsonTaggedAdt.PureDecoderWithConfig:
  case Weighted    extends MovingAverage("weighted")
  case Exponential extends MovingAverage("exponential")
  case Simple      extends MovingAverage("simple")
object MovingAverage:
  given JsonTaggedAdt.PureConfig[MovingAverage] = JsonTaggedAdt.PureConfig.Values[MovingAverage](
    mappings = Map(
      MovingAverage.Weighted.kind    -> JsonTaggedAdt.tagged[MovingAverage.Weighted.type],
      MovingAverage.Exponential.kind -> JsonTaggedAdt.tagged[MovingAverage.Exponential.type],
      MovingAverage.Simple.kind      -> JsonTaggedAdt.tagged[MovingAverage.Simple.type]
    )
  )

enum CompositeMovingAverage(val kind: String) derives JsonTaggedAdt.PureEncoderWithConfig, JsonTaggedAdt.PureDecoderWithConfig:
  case Triple  extends CompositeMovingAverage("triple")
  case Hull    extends CompositeMovingAverage("hull")
  case Nyquist extends CompositeMovingAverage("nyquist")
object CompositeMovingAverage:
  given JsonTaggedAdt.PureConfig[CompositeMovingAverage] = JsonTaggedAdt.PureConfig.Values[CompositeMovingAverage](
    mappings = Map(
      CompositeMovingAverage.Triple.kind  -> JsonTaggedAdt.tagged[CompositeMovingAverage.Triple.type],
      CompositeMovingAverage.Hull.kind    -> JsonTaggedAdt.tagged[CompositeMovingAverage.Hull.type],
      CompositeMovingAverage.Nyquist.kind -> JsonTaggedAdt.tagged[CompositeMovingAverage.Nyquist.type]
    )
  )

enum ValueSource(val kind: String) derives JsonTaggedAdt.PureEncoderWithConfig, JsonTaggedAdt.PureDecoderWithConfig:
  case Close extends ValueSource("close")
  case Open  extends ValueSource("open")
object ValueSource:
  given JsonTaggedAdt.PureConfig[ValueSource] = JsonTaggedAdt.PureConfig.Values[ValueSource](
    mappings = Map(
      ValueSource.Close.kind -> JsonTaggedAdt.tagged[ValueSource.Close.type],
      ValueSource.Open.kind  -> JsonTaggedAdt.tagged[ValueSource.Open.type]
    )
  )

enum ValueTransformation(val kind: String) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Sequenced(transformations: List[ValueTransformation]) extends ValueTransformation("sequenced")
  case EMA(length: Int)                                      extends ValueTransformation("ema")
  case HMA(length: Int)                                      extends ValueTransformation("hma")
  case NMA(
      length: Int,
      signalLength: Int,
      lambda: Double,
      maCalc: MovingAverage
  ) extends ValueTransformation("nma")

object ValueTransformation:
  given JsonTaggedAdt.Config[ValueTransformation] = JsonTaggedAdt.Config.Values[ValueTransformation](
    mappings = Map(
      "sequenced" -> JsonTaggedAdt.tagged[ValueTransformation.Sequenced],
      "ema"       -> JsonTaggedAdt.tagged[ValueTransformation.EMA],
      "hma"       -> JsonTaggedAdt.tagged[ValueTransformation.HMA],
      "nma"       -> JsonTaggedAdt.tagged[ValueTransformation.NMA]
    ),
    strict = true,
    typeFieldName = "kind"
  )

enum Indicator(val kind: String) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case TrendDirectionChange(
      source: ValueSource,
      transformation: ValueTransformation
  ) extends Indicator("trend-direction-change")

object Indicator:
  given JsonTaggedAdt.Config[Indicator] = JsonTaggedAdt.Config.Values[Indicator](
    mappings = Map(
      "trend-direction-change" -> JsonTaggedAdt.tagged[Indicator.TrendDirectionChange]
    ),
    strict = true,
    typeFieldName = "kind"
  )
