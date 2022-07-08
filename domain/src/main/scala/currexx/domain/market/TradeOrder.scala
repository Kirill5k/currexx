package currexx.domain.market

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
  enum Position derives JsonTaggedAdt.PureEncoderWithConfig, JsonTaggedAdt.PureDecoderWithConfig:
    case Buy, Sell

  object Position:
    given JsonTaggedAdt.PureConfig[Position] = JsonTaggedAdt.PureConfig.Values[Position](
      mappings = Map(
        "buy"  -> JsonTaggedAdt.tagged[Position.Buy.type],
        "sell" -> JsonTaggedAdt.tagged[Position.Sell.type]
      )
    )

  given JsonTaggedAdt.Config[TradeOrder] = JsonTaggedAdt.Config.Values[TradeOrder](
    mappings = Map(
      "exit"  -> JsonTaggedAdt.tagged[TradeOrder.Exit.type],
      "enter" -> JsonTaggedAdt.tagged[TradeOrder.Enter]
    ),
    strict = true,
    typeFieldName = "kind"
  )
}
