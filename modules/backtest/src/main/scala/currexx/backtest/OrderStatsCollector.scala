package currexx.backtest

import currexx.backtest.syntax.*
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.TradeOrder

import java.time.Instant
import scala.collection.immutable.ListMap

final case class OrderStats(
    total: Int = 0,
    buys: Int = 0,
    sells: Int = 0,
    losses: List[BigDecimal] = List.empty,
    totalProfit: BigDecimal = BigDecimal(0),
    biggestWin: BigDecimal = BigDecimal(0),
    biggestLoss: BigDecimal = BigDecimal(0),
    profitByMonth: Map[String, BigDecimal] = ListMap.empty
):
  def medianProfitByMonth: BigDecimal = profitByMonth.values.toList.median.roundTo(5)
  def meanProfitByMonth: BigDecimal   = profitByMonth.values.toList.mean.roundTo(5)
  def meanLoss: BigDecimal            = losses.mean.roundTo(5)
  def incBuy: OrderStats              = copy(total = total + 1, buys = buys + 1)
  def incSell: OrderStats             = copy(total = total + 1, sells = sells + 1)
  def close(profit: BigDecimal, time: Instant) =
    copy(
      losses = if (profit < BigDecimal(0)) profit :: losses else losses,
      totalProfit = totalProfit + profit,
      biggestWin = profit.max(biggestWin),
      biggestLoss = profit.min(biggestLoss),
      profitByMonth = {
        val date = time.toString.slice(0, 7)
        profitByMonth + (date -> (profitByMonth.getOrElse(date, BigDecimal(0)) + profit))
      }
    )
  override def toString: String =
    s"""OrderStats(
       |totalProfit=$totalProfit,
       |medianProfitByMonth=$medianProfitByMonth,
       |meanProfitByMonth=$meanProfitByMonth,
       |totalOrders=$total,
       |buys=$buys,
       |sells=$sells,
       |losses=${losses.size},
       |biggestWin=$biggestWin,
       |meanLoss=$meanLoss,
       |biggestLoss=$biggestLoss
       |)""".stripMargin.replaceAll("\n", "")

object OrderStatsCollector:
  def collect(orders: List[TradeOrderPlacement]): OrderStats =
    orders
      .foldLeft[(OrderStats, Option[TradeOrderPlacement])]((OrderStats(), None)) { case ((stats, prevOrder), currentOrder) =>
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
            (stats.incSell.close(sp - bp, currentOrder.time), Some(currentOrder))
          case (Some(TradeOrder.Enter(TradeOrder.Position.Buy, _, bp, _)), TradeOrder.Exit(_, ep)) =>
            (stats.close(ep - bp, currentOrder.time), None)

          case (Some(TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _)), TradeOrder.Enter(TradeOrder.Position.Sell, _, _, _)) =>
            (stats, prevOrder)
          case (Some(TradeOrder.Enter(TradeOrder.Position.Sell, _, sp, _)), TradeOrder.Enter(TradeOrder.Position.Buy, _, bp, _)) =>
            (stats.incBuy.close(sp - bp, currentOrder.time), Some(currentOrder))
          case (Some(TradeOrder.Enter(TradeOrder.Position.Sell, _, sp, _)), TradeOrder.Exit(_, ep)) =>
            (stats.close(sp - ep, currentOrder.time), None)
      }
      ._1
