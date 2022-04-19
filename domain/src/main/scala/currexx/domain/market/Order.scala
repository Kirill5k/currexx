package currexx.domain.market

import io.circe.syntax.*
import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, Json}

import scala.util.Try

enum Position:
  case Buy, Sell

object PositionKind:
  inline given Encoder[Position] = Encoder.encodeString.contramap(_.toString.toLowerCase)
  inline given Decoder[Position] = Decoder.decodeString.emap { p =>
    Try(Position.valueOf(p.toLowerCase.capitalize)).toOption
      .toRight(s"$p is not valid position kind. Accepted values: ${Position.values.map(_.toString.toLowerCase).mkString(", ")}")
  }

sealed trait Order(val kind: String):
  def currencyPair: CurrencyPair

object Order {
  final case class MarketOrder(
      currencyPair: CurrencyPair,
      position: Position,
      volume: BigDecimal,
      stopLoss: Option[BigDecimal],
      trailingStopLoss: Option[BigDecimal],
      takeProfit: Option[BigDecimal]
  ) extends Order("market")
      derives Codec.AsObject

  final case class CloseOrder(
      currencyPair: CurrencyPair
  ) extends Order("close")
      derives Codec.AsObject

  private val discriminatorField: String            = "kind"
  private def discriminatorJson(order: Order): Json = Map(discriminatorField -> order.kind).asJson

  inline given Decoder[Order] = Decoder.instance { c =>
    c.downField(discriminatorField).as[String].flatMap {
      case "market" => c.as[MarketOrder]
      case "close"  => c.as[CloseOrder]
      case kind     => Left(DecodingFailure(s"Unexpected order kind $kind", List(CursorOp.Field(discriminatorField))))
    }
  }
  inline given Encoder[Order] = Encoder.instance {
    case market: MarketOrder => market.asJson.deepMerge(discriminatorJson(market))
    case close: CloseOrder   => close.asJson.deepMerge(discriminatorJson(close))
  }
}
