package currexx.backtest

import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.TradeOrder

import java.time.Instant

final case class OrderStats(
    total: Int = 0,
    buys: Int = 0,
    sells: Int = 0,
    totalProfit: BigDecimal = BigDecimal(0),
    biggestWin: BigDecimal = BigDecimal(0),
    biggestLoss: BigDecimal = BigDecimal(0)
):
  def incBuy: OrderStats  = copy(total = total + 1, buys = buys + 1)
  def incSell: OrderStats = copy(total = total + 1, sells = sells + 1)
  def close(profit: BigDecimal) =
    copy(
      totalProfit = totalProfit + profit,
      biggestWin = profit.max(biggestWin),
      biggestLoss = profit.min(biggestLoss)
    )
  override def toString: String =
    s"""OrderStats(
       |totalProfit=$totalProfit,
       |totalOrders=$total,
       |buys=$buys,
       |sells=$sells,
       |biggestWin=$biggestWin,
       |biggestLoss=$biggestLoss
       |)""".stripMargin.replaceAll("\n", "")

object OrderStatsCollector:
  def collect(orders: List[TradeOrderPlacement]): OrderStats =
    orders
      .foldRight[(OrderStats, Option[TradeOrderPlacement])]((OrderStats(), None)) { case (currentOrder, (stats, prevOrder)) =>
        (prevOrder.map(_.order), currentOrder.order) match
          case (None, TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _))  => (stats.incBuy, Some(currentOrder))
          case (None, TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _)) => (stats.incSell, Some(currentOrder))
          case (None, TradeOrder.Exit(_, _))                               => (stats, None)

          case (Some(TradeOrder.Exit(_, _)), TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _))  => (stats.incBuy, Some(currentOrder))
          case (Some(TradeOrder.Exit(_, _)), TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _)) => (stats.incSell, Some(currentOrder))
          case (Some(TradeOrder.Exit(_, _)), TradeOrder.Exit(_, _))                               => (stats, None)

          case (Some(TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _)), TradeOrder.Enter(TradeOrder.Position.Buy, _, _, _)) =>
            (stats, prevOrder)
          case (Some(TradeOrder.Enter(TradeOrder.Position.Buy, _, bp, _)), TradeOrder.Enter(TradeOrder.Position.Sell, _, sp, _)) =>
            (stats.incSell.close(sp - bp), Some(currentOrder))
          case (Some(TradeOrder.Enter(TradeOrder.Position.Buy, _, bp, _)), TradeOrder.Exit(_, ep)) =>
            (stats.close(ep - bp), None)

          case (Some(TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _)), TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _)) =>
            (stats, prevOrder)
          case (Some(TradeOrder.Enter(TradeOrder.Position.Sell, _, sp, _)), TradeOrder.Enter(TradeOrder.Position.Buy, _, bp, _)) =>
            (stats.incBuy.close(sp - bp), Some(currentOrder))
          case (Some(TradeOrder.Enter(TradeOrder.Position.Sell, _, sp, _)), TradeOrder.Exit(_, ep)) =>
            (stats.close(sp - ep), None)
      }
      ._1
