package currexx.core.common.http

import squants.market.{Currency, defaultMoneyContext}
import sttp.tapir.Codec.PlainCodec
import sttp.tapir.{Codec, DecodeResult, ValidationError, Validator}

transparent trait TapirCodecs {

  private def decodeCurrency(code: String): DecodeResult[Currency] =
    Currency(code)(defaultMoneyContext).fold(DecodeResult.Error(code, _), DecodeResult.Value.apply)

  implicit val currencyCodec: PlainCodec[Currency] =
    Codec.string.mapDecode[Currency](decodeCurrency)(_.code)
}
