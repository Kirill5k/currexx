package currexx.core.trade

import currexx.core.market.MarketState
import currexx.domain.market.Indicator
import io.circe.{Decoder, Encoder}

enum TradeStrategy(val name: String):
  case Simple extends TradeStrategy("simple")

object TradeStrategy:
  inline given Encoder[TradeStrategy] = Encoder[String].contramap(_.name)
  inline given Decoder[TradeStrategy] =
    Decoder[String].emap(s => TradeStrategy.values.find(_.name == s).toRight(s"Unrecognized strategy $s"))

trait TradeStrategyExecutor:
  def analyze(state: MarketState, trigger: Indicator): Option[TradeStrategyExecutor.Outcome]

object TradeStrategyExecutor {
  enum Outcome:
    case Buy, Sell, Close

  def get(strategy: TradeStrategy): TradeStrategyExecutor =
    new TradeStrategyExecutor:
      override def analyze(state: MarketState, trigger: Indicator): Option[Outcome] = None
}
