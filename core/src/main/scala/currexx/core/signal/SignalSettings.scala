package currexx.core.signal

import currexx.domain.market.Indicator
import currexx.domain.types.EnumType
import currexx.domain.user.UserId

enum TriggerFrequency(val kind: String):
  case Continuously extends TriggerFrequency("continuously")
  case OncePerDay   extends TriggerFrequency("once-per-day")
object TriggerFrequency extends EnumType[TriggerFrequency](() => TriggerFrequency.values, _.kind)

final case class SignalSettings(
    userId: UserId,
    triggerFrequency: TriggerFrequency,
    indicators: List[Indicator]
)
