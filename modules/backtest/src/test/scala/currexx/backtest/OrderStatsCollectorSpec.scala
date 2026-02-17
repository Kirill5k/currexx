package currexx.backtest

import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, Currency}
import currexx.domain.market.TradeOrder.{Position, Enter, Exit}
import currexx.domain.user.UserId
import currexx.clients.broker.BrokerParameters
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers
import java.time.Instant

class OrderStatsCollectorSpec extends AnyWordSpec with Matchers {

  val uid = UserId("user-1")
  val brokerParams = BrokerParameters.Oanda("key", true, "account")
  val cp = CurrencyPair(Currency.EUR, Currency.USD)
  val time = Instant.now()

  def mkEnter(pos: Position, price: Double): TradeOrderPlacement =
    TradeOrderPlacement(uid, Enter(pos, cp, BigDecimal(price), BigDecimal(1)), brokerParams, time)

  def mkExit(price: Double): TradeOrderPlacement =
    TradeOrderPlacement(uid, Exit(cp, BigDecimal(price)), brokerParams, time)

  "OrderStatsCollector" should {
    "calculate stats for a single Buy trade" in {
      val orders = List(
        mkEnter(Position.Buy, 100),
        mkExit(110)
      )
      val stats = OrderStatsCollector.collect(orders)
      
      stats.total mustBe 1
      stats.buys mustBe 1
      stats.sells mustBe 0
      stats.totalProfit mustBe BigDecimal(10)
      stats.winLossRatio mustBe BigDecimal(1) // 1 win, 0 losses -> 1 total
    }

    "calculate stats for a single Sell trade" in {
      val orders = List(
        mkEnter(Position.Sell, 100),
        mkExit(90)
      )
      val stats = OrderStatsCollector.collect(orders)

      stats.total mustBe 1
      stats.buys mustBe 0
      stats.sells mustBe 1
      stats.totalProfit mustBe BigDecimal(10)
    }

    "calculate stats for reverse trade (Buy -> Sell)" in {
      // Buy @ 100. Then Sell @ 90 (Close Buy, Open Sell).
      val orders = List(
        mkEnter(Position.Buy, 100),
        mkEnter(Position.Sell, 90)
      )
      val stats = OrderStatsCollector.collect(orders)

      // First trade: Buy 100 -> Sell 90. Loss 10.
      // Second trade: Sell 90 -> Still open.
      // Stats counts closed trades? Or opened trades?
      // Logic: (None, Buy) -> stats.incBuy.
      // (Buy, Sell) -> stats.incSell.close(sell-buy).
      // So total increments are: incBuy (1), then incSell (2).
      // Buys: 1, Sells: 1. Total: 2.
      // Profit: 90 - 100 = -10.
      
      stats.total mustBe 2
      stats.buys mustBe 1
      stats.sells mustBe 1
      stats.totalProfit mustBe BigDecimal(-10)
      stats.losses.size mustBe 1
      stats.losses.head mustBe BigDecimal(-10)
    }

    "handle winLossRatio correctly" in {
      // 2 wins, 0 losses
      val orders1 = List(
        mkEnter(Position.Buy, 100), mkExit(110),
        mkEnter(Position.Buy, 100), mkExit(110)
      )
      OrderStatsCollector.collect(orders1).winLossRatio mustBe BigDecimal(2)

      // 1 win, 1 loss
      val orders2 = List(
        mkEnter(Position.Buy, 100), mkExit(110), // +10
        mkEnter(Position.Buy, 100), mkExit(90)   // -10
      )
      OrderStatsCollector.collect(orders2).winLossRatio mustBe BigDecimal(1) // (2-1)/1 = 1
      
      // 0 wins, 2 losses
      val orders3 = List(
        mkEnter(Position.Buy, 100), mkExit(90),
        mkEnter(Position.Buy, 100), mkExit(90)
      )
      OrderStatsCollector.collect(orders3).winLossRatio mustBe BigDecimal(0) // (2-2)/2 = 0
    }
  }
}
