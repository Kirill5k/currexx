package currexx.core.signal

import currexx.domain.user.UserId
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.signal.{Condition, Indicator}
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
