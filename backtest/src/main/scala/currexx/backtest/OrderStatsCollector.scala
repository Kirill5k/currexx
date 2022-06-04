package currexx.backtest

import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.TradeOrder

final case class OrderStats(
    total: Int = 0,
    buys: Int = 0,
    sells: Int = 0,
    closes: Int = 0,
    biggestWin: Option[BigDecimal] = None,
    biggestLoss: Option[BigDecimal] = None,
    profit: BigDecimal = BigDecimal(0)
):
  def incBuy: OrderStats   = copy(total = total + 1, buys = buys + 1)
  def incSell: OrderStats  = copy(total = total + 1, sells = sells + 1)
  def incClose: OrderStats = copy(total = total + 1, closes = closes + 1)
  def addProfit(moreProfit: BigDecimal) =
    copy(
      profit = profit + moreProfit,
      biggestWin = biggestWin.orElse(Some(moreProfit)).map(w => if (moreProfit > w) moreProfit else w),
      biggestLoss = biggestLoss.orElse(Some(moreProfit)).map(l => if (moreProfit < l) moreProfit else l)
    )

object OrderStatsCollector {

  def collect(orders: List[TradeOrderPlacement]): OrderStats =
    orders.reverse
      .foldRight[(OrderStats, Option[TradeOrderPlacement])]((OrderStats(), None)) { case (currentOrder, (stats, prevOrder)) =>
        (prevOrder.map(_.order), currentOrder.order) match
          case (None, TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _, _))  => (stats.incBuy, Some(currentOrder))
          case (None, TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _, _)) => (stats.incBuy, Some(currentOrder))
          case (None, TradeOrder.Exit)                                        => (stats.incClose, None)

          case (Some(TradeOrder.Exit), TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _, _))  => (stats.incBuy, Some(currentOrder))
          case (Some(TradeOrder.Exit), TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _, _)) => (stats.incBuy, Some(currentOrder))
          case (Some(TradeOrder.Exit), TradeOrder.Exit)                                       => (stats.incClose, None)

          case (Some(TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _, _)), TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _, _)) =>
            (stats, prevOrder)
          case (Some(TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _, _)), TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _, _)) =>
            (stats.incSell.addProfit(currentOrder.currentPrice.close - prevOrder.get.currentPrice.close), Some(currentOrder))
          case (Some(TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _, _)), TradeOrder.Exit) =>
            (stats.incClose.addProfit(currentOrder.currentPrice.close - prevOrder.get.currentPrice.close), None)

          case (Some(TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _, _)), TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _, _)) =>
            (stats, prevOrder)
          case (Some(TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _, _)), TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _, _)) =>
            (stats.incBuy.addProfit(prevOrder.get.currentPrice.close - currentOrder.currentPrice.close), Some(currentOrder))
          case (Some(TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _, _)), TradeOrder.Exit) =>
            (stats.incClose.addProfit(prevOrder.get.currentPrice.close - currentOrder.currentPrice.close), None)
      }
      ._1
}
