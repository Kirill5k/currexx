package currexx.core.signal

import currexx.domain.user.UserId
import currexx.domain.market.{Condition, CurrencyPair, Indicator}
import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder}
import io.circe.syntax.*

import java.time.Instant
import scala.util.Try

final case class Signal(
    userId: UserId,
    currencyPair: CurrencyPair,
    indicator: Indicator,
    condition: Condition,
    time: Instant
) derives Codec.AsObject
