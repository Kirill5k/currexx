package currexx.domain.market

import currexx.domain.types.EnumType
import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*
import org.latestbit.circe.adt.codec.*

sealed trait TradeOrder(val kind: String):
  def isEnter: Boolean
  def currencyPair: CurrencyPair
  def price: BigDecimal

object TradeOrder {
  object Position extends EnumType[Position](() => Position.values)
  enum Position:
    case Buy, Sell

  final case class Enter(
      position: TradeOrder.Position,
      currencyPair: CurrencyPair,
      price: BigDecimal,
      volume: BigDecimal
  ) extends TradeOrder("enter") derives Codec.AsObject:
    def isEnter: Boolean = true

  final case class Exit(
      currencyPair: CurrencyPair,
      price: BigDecimal
  ) extends TradeOrder("exit") derives Codec.AsObject:
    def isEnter: Boolean = false

  inline given Decoder[TradeOrder] = Decoder.instance { c =>
    c.downField("kind").as[String].flatMap {
      case "exit"  => c.as[Exit]
      case "enter" => c.as[Enter]
      case kind    => Left(DecodingFailure(s"Unexpected trade-order kind $kind", List(CursorOp.Field("kind"))))
    }
  }

  inline given Encoder[TradeOrder] = Encoder.instance {
    case enter: Enter => enter.asJsonObject.add("kind", Json.fromString(enter.kind)).asJson
    case exit: Exit   => exit.asJsonObject.add("kind", Json.fromString(exit.kind)).asJson
  }
}

final case class OpenedTradeOrder(
    currencyPair: CurrencyPair,
    position: TradeOrder.Position,
    openPrice: BigDecimal,
    currentPrice: BigDecimal,
    volume: BigDecimal,
    profit: BigDecimal
)

enum OrderPlacementStatus derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Success
  case Pending
  case NoPosition
  case Cancelled(reason: String)

object OrderPlacementStatus:
  given JsonTaggedAdt.Config[OrderPlacementStatus] = JsonTaggedAdt.Config.Values[OrderPlacementStatus](
    mappings = Map(
      "success"    -> JsonTaggedAdt.tagged[OrderPlacementStatus.Success.type],
      "pending"    -> JsonTaggedAdt.tagged[OrderPlacementStatus.Pending.type],
      "noPosition" -> JsonTaggedAdt.tagged[OrderPlacementStatus.NoPosition.type],
      "cancelled"  -> JsonTaggedAdt.tagged[OrderPlacementStatus.Cancelled]
    ),
    strict = true,
    typeFieldName = "kind"
  )
