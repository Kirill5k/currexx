package currexx.core.signal

import currexx.domain.user.UserId
import currexx.domain.market.{Condition, CurrencyPair, Indicator, Interval}
import io.circe.Codec

import java.time.Instant

final case class Signal(
    userId: UserId,
    currencyPair: CurrencyPair,
    interval: Interval,
    condition: Condition,
    triggeredBy: Indicator,
    time: Instant
) derives Codec.AsObject
