package currexx.core.signal

import org.latestbit.circe.adt.codec.*
import currexx.domain.market.v2.Indicator
import currexx.domain.user.UserId

enum TriggerFrequency derives JsonTaggedAdt.PureEncoderWithConfig, JsonTaggedAdt.PureDecoderWithConfig:
  case Continuously, OncePerDay

object TriggerFrequency:
  given JsonTaggedAdt.PureConfig[TriggerFrequency] = JsonTaggedAdt.PureConfig.Values[TriggerFrequency](
    mappings = Map(
      "continuously" -> JsonTaggedAdt.tagged[TriggerFrequency.Continuously.type],
      "once-per-day" -> JsonTaggedAdt.tagged[TriggerFrequency.OncePerDay.type]
    )
  )

final case class SignalSettings(
    userId: UserId,
    triggerFrequency: TriggerFrequency,
    indicators: List[Indicator]
)
