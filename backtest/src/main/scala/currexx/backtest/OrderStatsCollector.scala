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
    profit: BigDecimal = BigDecimal(0),
                           ):
  def incBuy: OrderStats                = copy(total = total + 1, buys = buys + 1)
  def incSell: OrderStats               = copy(total = total + 1, sells = sells + 1)
  def incClose: OrderStats              = copy(total = total + 1, closes = closes + 1)
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
        (prevOrder, currentOrder) match
          case (Some(prev), curr) if prev.isBuy && curr.isSell =>
            (stats.incSell.addProfit(curr.currentPrice.close - prev.currentPrice.close), Some(curr))
          case (Some(prev), curr) if prev.isSell && curr.isBuy =>
            (stats.incClose.addProfit(prev.currentPrice.close - curr.currentPrice.close), Some(curr))
          case (Some(prev), curr) if prev.isBuy && curr.isClose =>
            (stats.incClose.addProfit(curr.currentPrice.close - prev.currentPrice.close), None)
          case (Some(prev), curr) if prev.isSell && curr.isClose =>
            (stats.incClose.addProfit(prev.currentPrice.close - curr.currentPrice.close), None)
          case (Some(prev), curr) if (prev.isSell && curr.isSell) || (prev.isBuy && curr.isBuy) =>
            (stats, Some(prev))
          case (Some(_), curr) =>
            (stats, Some(curr))
          case (None, curr) if curr.isBuy =>
            (stats.incBuy, Some(curr))
          case (None, curr) if curr.isSell =>
            (stats.incSell, Some(curr))
          case (None, curr) =>
            (stats.incClose, Some(curr))
      }
      ._1

  extension (top: TradeOrderPlacement)
    def isBuy: Boolean = top.order match
      case TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _, _) => true
      case _                                                     => false
    def isSell: Boolean = top.order match
      case TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _, _) => true
      case _                                                      => false
    def isClose: Boolean = top.order match
      case TradeOrder.Enter(_, _, _, _, _) => false
      case TradeOrder.Exit                 => true
}
