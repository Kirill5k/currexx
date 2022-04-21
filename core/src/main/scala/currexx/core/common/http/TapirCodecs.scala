package currexx.core.common.http

import squants.market.{Currency, defaultMoneyContext}
import sttp.tapir.Codec.PlainCodec
import sttp.tapir.{Codec, DecodeResult, ValidationError, Validator}

transparent trait TapirCodecs {

  private val validCurrency: Validator[String] = Validator.custom { curr =>
    lazy val error = ValidationError.Custom(curr, s"Invalid currency code: $curr", Nil)
    Option.when(!defaultMoneyContext.currencyMap.keys.toSet.contains(curr))(error).toList
  }

  private def decodeCurrency(code: String): DecodeResult[Currency] =
    Currency(code)(defaultMoneyContext).fold(DecodeResult.Error(code, _), DecodeResult.Value.apply)

  implicit val currencyCodec: PlainCodec[Currency] =
    Codec.string.validate(validCurrency).mapDecode[Currency](decodeCurrency)(_.code)
}
