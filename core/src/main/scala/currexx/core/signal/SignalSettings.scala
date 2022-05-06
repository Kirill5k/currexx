package currexx.core.signal

import io.circe.{Decoder, Encoder}
import currexx.domain.market.{CurrencyPair, Indicator, IndicatorParameters}
import currexx.domain.user.UserId

enum TriggerFrequency(val kind: String):
  case Continuously extends TriggerFrequency("continuously")
  case OncePerDay   extends TriggerFrequency("once-per-day")

object TriggerFrequency:
  inline given Encoder[TriggerFrequency] = Encoder[String].contramap(_.kind)
  inline given Decoder[TriggerFrequency] =
    Decoder[String].emap(tf => TriggerFrequency.values.find(_.kind == tf).toRight(s"Unrecognized trigger frequency $tf"))

final case class SignalSettings(
    userId: UserId,
    triggerFrequency: TriggerFrequency,
    indicators: List[IndicatorParameters]
)

object SignalSettings:
  def default(userId: UserId): SignalSettings =
    SignalSettings(userId, TriggerFrequency.OncePerDay, List(IndicatorParameters.MACD(), IndicatorParameters.RSI()))
