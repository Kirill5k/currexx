package currexx.domain.market

import io.circe.{Codec, Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import org.latestbit.circe.adt.codec.*

enum Indicator(val kind: String):
  case MACD       extends Indicator("macd")
  case RSI        extends Indicator("rsi")
  case HMA        extends Indicator("hma")
  case NMA        extends Indicator("nma")
  case Stochastic extends Indicator("stochastic")

object Indicator:
  inline given Decoder[Indicator]    = Decoder[String].emap(i => Indicator.values.find(_.kind == i).toRight(s"Unrecognized indicator $i"))
  inline given Encoder[Indicator]    = Encoder[String].contramap(_.kind)
  inline given KeyDecoder[Indicator] = KeyDecoder.instance(i => Indicator.values.find(_.kind == i))
  inline given KeyEncoder[Indicator] = KeyEncoder.instance(_.kind)

enum IndicatorParameters(val indicator: Indicator) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case MACD(
      fastLength: Int = 12,
      slowLength: Int = 26,
      signalSmoothing: Int = 9
  ) extends IndicatorParameters(Indicator.MACD)

  case RSI(
      length: Int = 14,
      upperLine: Int = 70,
      lowerLine: Int = 30
  ) extends IndicatorParameters(Indicator.RSI)

  case Stochastic(
      length: Int = 14,
      slowKLength: Int = 3,
      slowDLength: Int = 3,
      upperLine: Int = 20,
      lowerLine: Int = 80
  ) extends IndicatorParameters(Indicator.Stochastic)

  case HMA(
      length: Int = 16
  ) extends IndicatorParameters(Indicator.HMA)

  case NMA(
      length: Int = 12,
      signalLength: Int = 6,
      lambda: Double = 4.2
  ) extends IndicatorParameters(Indicator.NMA)

object IndicatorParameters:
  given JsonTaggedAdt.Config[IndicatorParameters] = JsonTaggedAdt.Config.Values[IndicatorParameters](
    mappings = Map(
      Indicator.MACD.kind       -> JsonTaggedAdt.tagged[IndicatorParameters.MACD],
      Indicator.RSI.kind        -> JsonTaggedAdt.tagged[IndicatorParameters.RSI],
      Indicator.HMA.kind        -> JsonTaggedAdt.tagged[IndicatorParameters.HMA],
      Indicator.NMA.kind        -> JsonTaggedAdt.tagged[IndicatorParameters.NMA],
      Indicator.Stochastic.kind -> JsonTaggedAdt.tagged[IndicatorParameters.Stochastic]
    ),
    strict = true,
    typeFieldName = "indicator"
  )
