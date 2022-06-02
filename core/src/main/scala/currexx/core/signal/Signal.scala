package currexx.core.signal

import currexx.domain.user.UserId
import currexx.domain.market.{Condition, CurrencyPair, Indicator}
import io.circe.Codec

import java.time.Instant

final case class Signal(
    userId: UserId,
    currencyPair: CurrencyPair,
    condition: Condition,
    triggeredBy: Indicator,
    time: Instant
) derives Codec.AsObject
