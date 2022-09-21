package currexx.core.signal

import currexx.domain.market.Indicator
import currexx.domain.types.EnumType
import currexx.domain.user.UserId

object TriggerFrequency extends EnumType[TriggerFrequency](() => TriggerFrequency.values, _.print)
enum TriggerFrequency:
  case Continuously, OncePerDay
  
final case class SignalSettings(
    userId: UserId,
    triggerFrequency: TriggerFrequency,
    indicators: List[Indicator]
)
