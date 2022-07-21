package currexx.core.common.http

import squants.market.{Currency, defaultMoneyContext}
import sttp.tapir.Codec.PlainCodec
import sttp.tapir.{Codec, DecodeResult, ValidationError, Validator}
import currexx.core.common.time.*

import java.time.Instant

transparent trait TapirCodecs {

  private def decodeCurrency(code: String): DecodeResult[Currency] =
    Currency(code)(defaultMoneyContext).fold(DecodeResult.Error(code, _), DecodeResult.Value.apply)

  inline given PlainCodec[Currency] = Codec.string.mapDecode[Currency](decodeCurrency)(_.code)

  inline given instantCodec: PlainCodec[Instant] =
    Codec.string.mapDecode(d => d.toInstant.fold(DecodeResult.Error(d, _), DecodeResult.Value(_)))(_.toString)
}
