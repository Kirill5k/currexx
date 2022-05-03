package currexx.backtest

import currexx.core.trade.TradeOrderPlacement

final case class OrderStats(
    total: Int,
    buys: Int,
    sells: Int,
    closes: Int
)

object OrderStatsCollector {

  def collect(orders: List[TradeOrderPlacement]): OrderStats = ???
}
