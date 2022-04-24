package currexx.domain.market

import io.circe.syntax.*
import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, Json}

import scala.util.Try

sealed trait Order(val kind: String):
  def currencyPair: CurrencyPair

object Order {
  enum Position:
    case Buy, Sell

  object Position:
    inline given Encoder[Position] = Encoder.encodeString.contramap(_.toString.toLowerCase)
    inline given Decoder[Position] = Decoder.decodeString.emap { p =>
      Try(Position.valueOf(p.toLowerCase.capitalize)).toOption
        .toRight(s"$p is not valid position kind. Accepted values: ${Position.values.map(_.toString.toLowerCase).mkString(", ")}")
    }

  final case class EnterMarket(
      currencyPair: CurrencyPair,
      position: Position,
      volume: BigDecimal,
      stopLoss: Option[BigDecimal],
      trailingStopLoss: Option[BigDecimal],
      takeProfit: Option[BigDecimal]
  ) extends Order("enter")
      derives Codec.AsObject

  final case class ExitMarket(
      currencyPair: CurrencyPair
  ) extends Order("exit")
      derives Codec.AsObject

  private val discriminatorField: String            = "kind"
  private def discriminatorJson(order: Order): Json = Map(discriminatorField -> order.kind).asJson

  inline given Decoder[Order] = Decoder.instance { c =>
    c.downField(discriminatorField).as[String].flatMap {
      case "enter" => c.as[EnterMarket]
      case "exit"  => c.as[ExitMarket]
      case kind    => Left(DecodingFailure(s"Unexpected order kind $kind", List(CursorOp.Field(discriminatorField))))
    }
  }
  inline given Encoder[Order] = Encoder.instance {
    case enter: EnterMarket => enter.asJson.deepMerge(discriminatorJson(enter))
    case exit: ExitMarket   => exit.asJson.deepMerge(discriminatorJson(exit))
  }
}
