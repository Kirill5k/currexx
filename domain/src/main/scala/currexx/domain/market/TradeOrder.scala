package currexx.domain.market

import currexx.domain.types.EnumType
import org.latestbit.circe.adt.codec.*

import scala.util.Try

enum TradeOrder derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Exit
  case Enter(
      position: TradeOrder.Position,
      volume: BigDecimal,
      stopLoss: Option[BigDecimal] = None,
      trailingStopLoss: Option[BigDecimal] = None,
      takeProfit: Option[BigDecimal] = None
  )

object TradeOrder {
  object Position extends EnumType[Position](() => Position.values, _.toString.toLowerCase)
  enum Position:
    case Buy, Sell
  
  given JsonTaggedAdt.Config[TradeOrder] = JsonTaggedAdt.Config.Values[TradeOrder](
    mappings = Map(
      "exit"  -> JsonTaggedAdt.tagged[TradeOrder.Exit.type],
      "enter" -> JsonTaggedAdt.tagged[TradeOrder.Enter]
    ),
    strict = true,
    typeFieldName = "kind"
  )
}
