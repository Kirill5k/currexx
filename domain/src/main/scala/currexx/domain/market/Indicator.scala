package currexx.domain.market

import io.circe.{Codec, Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import io.circe.syntax.*

enum Indicator(val kind: String):
  case MACD       extends Indicator("macd")
  case RSI        extends Indicator("rsi")
  case HMA        extends Indicator("hma")
  case Stochastic extends Indicator("stochastic")

object Indicator:
  inline given Decoder[Indicator]    = Decoder[String].emap(i => Indicator.values.find(_.kind == i).toRight(s"Unrecognized indicator $i"))
  inline given Encoder[Indicator]    = Encoder[String].contramap(_.kind)
  inline given KeyDecoder[Indicator] = KeyDecoder.instance(i => Indicator.values.find(_.kind == i))
  inline given KeyEncoder[Indicator] = KeyEncoder.instance(_.kind)

sealed trait IndicatorParameters(val indicator: Indicator)

object IndicatorParameters {
  final case class KalmanSmoothing(
      alpha: Double,
      betta: Double,
      gamma: Double
  ) derives Codec.AsObject

  final case class MACD(
      fastLength: Int = 12,
      slowLength: Int = 26,
      signalSmoothing: Int = 9
  ) extends IndicatorParameters(Indicator.MACD)
      derives Codec.AsObject

  final case class RSI(
      length: Int = 14,
      upperLine: Int = 70,
      lowerLine: Int = 30
  ) extends IndicatorParameters(Indicator.RSI)
      derives Codec.AsObject

  final case class Stochastic(
      length: Int = 14,
      slowKLength: Int = 3,
      slowDLength: Int = 3,
      upperLine: Int = 20,
      lowerLine: Int = 80
  ) extends IndicatorParameters(Indicator.Stochastic)
      derives Codec.AsObject

  final case class HMA(
      length: Int = 16,
      kalmanSmoothing: Option[KalmanSmoothing] = None
  ) extends IndicatorParameters(Indicator.HMA)
      derives Codec.AsObject

  private val discriminatorField: String                       = "indicator"
  private def discriminatorJson(ip: IndicatorParameters): Json = Map(discriminatorField -> ip.indicator).asJson

  inline given Decoder[IndicatorParameters] = Decoder.instance { ip =>
    ip.downField(discriminatorField).as[Indicator].flatMap {
      case Indicator.MACD       => ip.as[IndicatorParameters.MACD]
      case Indicator.RSI        => ip.as[IndicatorParameters.RSI]
      case Indicator.HMA        => ip.as[IndicatorParameters.HMA]
      case Indicator.Stochastic => ip.as[IndicatorParameters.Stochastic]
    }
  }
  inline given Encoder[IndicatorParameters] = Encoder.instance {
    case macd: IndicatorParameters.MACD        => macd.asJson.deepMerge(discriminatorJson(macd))
    case rsi: IndicatorParameters.RSI          => rsi.asJson.deepMerge(discriminatorJson(rsi))
    case hma: IndicatorParameters.HMA          => hma.asJson.deepMerge(discriminatorJson(hma))
    case stock: IndicatorParameters.Stochastic => stock.asJson.deepMerge(discriminatorJson(stock))
  }
}
