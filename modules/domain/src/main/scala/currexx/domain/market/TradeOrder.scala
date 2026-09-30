package currexx.domain.market

import cats.data.NonEmptyList
import currexx.domain.JsonSyntax
import currexx.domain.types.EnumType
import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder}
import org.latestbit.circe.adt.codec.*

import java.time.Instant

sealed trait TradeOrder(val kind: String):
  def isEnter: Boolean
  def currencyPair: CurrencyPair
  def price: BigDecimal

object TradeOrder extends JsonSyntax {
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
    case enter: Enter => enter.asJsonWithKind(enter.kind)
    case exit: Exit   => exit.asJsonWithKind(exit.kind)
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

final case class OrderExecution(
    price: BigDecimal,
    time: Instant,
    volume: BigDecimal,
    orderId: String,
    transactionId: String,
    tradeIds: List[String]
) derives Codec.AsObject

final case class OrderRef(clientOrderId: String, brokerOrderId: Option[String])

enum OrderPlacementResult:
  // An entry has a single execution; closing a hedged position fills each side separately
  case Filled(fills: NonEmptyList[OrderExecution])
  // The broker confirmed the fill, but its execution details could not be retrieved
  case FilledWithoutExecution(brokerOrderId: Option[String])
  case Pending(ref: OrderRef)
  case Cancelled(reason: String)
  case NoPosition

  def status: OrderPlacementStatus = this match
    case Filled(_) | FilledWithoutExecution(_) => OrderPlacementStatus.Success
    case Pending(_)                            => OrderPlacementStatus.Pending
    case Cancelled(reason)                     => OrderPlacementStatus.Cancelled(reason)
    case NoPosition                            => OrderPlacementStatus.NoPosition

  def executions: List[OrderExecution] = this match
    case Filled(fills) => fills.toList
    case _             => Nil

object OrderPlacementResult:
  def filled(execution: OrderExecution): OrderPlacementResult = Filled(NonEmptyList.one(execution))

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
