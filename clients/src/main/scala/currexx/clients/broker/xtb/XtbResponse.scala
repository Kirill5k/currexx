package currexx.clients.broker.xtb

import cats.syntax.functor.*
import cats.syntax.either.*
import currexx.domain.errors.AppError
import currexx.domain.market.CurrencyPair
import io.circe.{Codec, Decoder}
import io.circe.parser.*

sealed trait XtbResponse
object XtbResponse {
  case object Void                                              extends XtbResponse
  final case class Login(streamSessionId: String)               extends XtbResponse derives Codec.AsObject
  final case class Error(errorCode: String, errorDescr: String) extends XtbResponse derives Codec.AsObject

  final case class SymbolData(ask: BigDecimal, bid: BigDecimal) derives Codec.AsObject
  final case class SymbolInfo(returnData: SymbolData) extends XtbResponse derives Codec.AsObject

  final case class OrderData(order: Long) derives Codec.AsObject
  final case class OrderPlacement(returnData: OrderData) extends XtbResponse derives Codec.AsObject

  final case class TradeData(
      position: Long,
      symbol: String,
      close_price: BigDecimal,
      closed: Boolean,
      volume: BigDecimal
  ) derives Codec.AsObject
  final case class Trades(returnData: List[TradeData]) extends XtbResponse derives Codec.AsObject

  given Decoder[XtbResponse] = List[Decoder[XtbResponse]](
    Decoder[Error].widen,
    Decoder[OrderPlacement].widen,
    Decoder[SymbolInfo].widen,
    Decoder[Trades].widen,
    Decoder[Login].widen
  ).reduceLeft(_ or _)

  def fromJson(json: String): Either[AppError.JsonParsingFailure, XtbResponse] =
    decode[XtbResponse](json).leftMap(e => AppError.JsonParsingFailure(json, s"Failed to parse XTB response: ${e.getMessage}"))
}
