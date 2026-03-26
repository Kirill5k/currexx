package currexx.backtest

import currexx.backtest.syntax.*
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.TradeOrder as TO

import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter

final case class OrderStats(
    total: Int = 0,
    buys: Int = 0,
    sells: Int = 0,
    lossCount: Int = 0,
    lossTotal: Double = 0.0,
    totalProfit: BigDecimal = BigDecimal(0),
    biggestWin: BigDecimal = BigDecimal(0),
    biggestLoss: BigDecimal = BigDecimal(0),
    profitByMonth: Map[String, BigDecimal] = Map.empty
):
  def medianProfitByMonth: BigDecimal          = profitByMonth.values.toList.median.roundTo(5)
  def meanProfitByMonth: BigDecimal            = profitByMonth.values.toList.mean.roundTo(5)
  def meanLoss: BigDecimal                     = if (lossCount == 0) BigDecimal(0) else BigDecimal(lossTotal / lossCount)
  def incBuy: OrderStats                       = copy(total = total + 1, buys = buys + 1)
  def incSell: OrderStats                      = copy(total = total + 1, sells = sells + 1)
  def close(profit: BigDecimal, time: Instant) =
    copy(
      lossCount = if (profit < BigDecimal(0)) lossCount + 1 else lossCount,
      lossTotal = if (profit < BigDecimal(0)) lossTotal + profit.toDouble else lossTotal,
      totalProfit = totalProfit + profit,
      biggestWin = profit.max(biggestWin),
      biggestLoss = profit.min(biggestLoss),
      profitByMonth = {
        val date = OrderStats.monthFormatter.format(time)
        profitByMonth + (date -> (profitByMonth.getOrElse(date, BigDecimal(0)) + profit))
      }
    )
  def winLossRatio: BigDecimal =
    if (lossCount == 0) if (total == 0) BigDecimal(0) else BigDecimal(total)
    else (BigDecimal(total - lossCount) / BigDecimal(lossCount)).roundTo(5)

  override def toString: String =
    s"""OrderStats(
       |totalProfit=$totalProfit,
       |totalOrders=$total,
       |winLossRatio=$winLossRatio,
       |meanProfitByMonth=$meanProfitByMonth,
       |medianProfitByMonth=$medianProfitByMonth,
       |biggestWin=$biggestWin,
       |biggestLoss=$biggestLoss,
       |meanLoss=$meanLoss,
       |buys=$buys,
       |sells=$sells,
       |losses=$lossCount
       |)""".stripMargin.replaceAll("\n", "")

object OrderStats:
  private[backtest] val monthFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM").withZone(ZoneOffset.UTC)

object OrderStatsCollector:
  def collect(orders: List[TradeOrderPlacement]): OrderStats =
    orders
      .foldLeft[(OrderStats, Option[TradeOrderPlacement])]((OrderStats(), None)) { case ((stats, openPosition), currentOrder) =>
        (openPosition.map(_.order), currentOrder.order) match {
          // No open position - only Enter orders are valid
          case (None, TO.Enter(TO.Position.Buy, _, _, _))  => (stats.incBuy, Some(currentOrder))
          case (None, TO.Enter(TO.Position.Sell, _, _, _)) => (stats.incSell, Some(currentOrder))
          case (None, TO.Exit(_, _))                       => (stats, None) // Invalid: exit without enter

          // Previous order was Exit - only Enter orders are valid
          case (Some(TO.Exit(_, _)), TO.Enter(TO.Position.Buy, _, _, _))  => (stats.incBuy, Some(currentOrder))
          case (Some(TO.Exit(_, _)), TO.Enter(TO.Position.Sell, _, _, _)) => (stats.incSell, Some(currentOrder))
          case (Some(TO.Exit(_, _)), TO.Exit(_, _))                       => (stats, None) // Invalid: exit after exit

          // Open Buy position
          case (Some(TO.Enter(TO.Position.Buy, _, _, _)), TO.Enter(TO.Position.Buy, _, _, _)) =>
            // Replace previous buy with new buy (no trade completion)
            (stats, Some(currentOrder))
          case (Some(TO.Enter(TO.Position.Buy, _, buyPrice, _)), TO.Enter(TO.Position.Sell, _, sellPrice, _)) =>
            // Close buy position and open sell position
            (stats.incSell.close(sellPrice - buyPrice, currentOrder.time), Some(currentOrder))
          case (Some(TO.Enter(TO.Position.Buy, _, buyPrice, _)), TO.Exit(_, exitPrice)) =>
            // Close buy position
            (stats.close(exitPrice - buyPrice, currentOrder.time), None)

          // Open Sell position
          case (Some(TO.Enter(TO.Position.Sell, _, _, _)), TO.Enter(TO.Position.Sell, _, _, _)) =>
            // Replace previous sell with new sell (no trade completion)
            (stats, Some(currentOrder))
          case (Some(TO.Enter(TO.Position.Sell, _, sellPrice, _)), TO.Enter(TO.Position.Buy, _, buyPrice, _)) =>
            // Close sell position and open buy position
            (stats.incBuy.close(sellPrice - buyPrice, currentOrder.time), Some(currentOrder))
          case (Some(TO.Enter(TO.Position.Sell, _, sellPrice, _)), TO.Exit(_, exitPrice)) =>
            // Close sell position
            (stats.close(sellPrice - exitPrice, currentOrder.time), None)
        }
      }
      ._1
