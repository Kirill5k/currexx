package currexx.backtest

import cats.data.NonEmptyList
import currexx.clients.broker.BrokerParameters
import currexx.backtest.types.given
import currexx.domain.market.{Currency, CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import currexx.domain.market.TradeOrder.{Enter, Exit, Position}
import currexx.domain.user.UserId
import currexx.core.trade.TradeOrderPlacement
import eu.timepit.refined.types.numeric.{NonNegBigDecimal, PosBigDecimal}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import kirill5k.common.syntax.time.*

import java.time.Instant
import scala.concurrent.duration.*

class OrderStatsCollectorSpec extends AnyWordSpec with Matchers {

  val uid          = UserId("user-1")
  val brokerParams = BrokerParameters.Oanda("key", true, "account")
  val cp           = CurrencyPair(Currency.EUR, Currency.USD)
  val start        = Instant.parse("2025-01-01T00:00:00Z")
  val noCosts      = RiskSettings(
    initialBalance = BigDecimal(1000),
    unitsPerLot = BigDecimal(1),
    transactionCosts = TransactionCosts(
      spreadPips = BigDecimal(0),
      slippagePipsPerSide = BigDecimal(0),
      commissionPerTrade = BigDecimal(0)
    )
  )

  def mkEnter(pos: Position, price: Double, hour: Long, volume: BigDecimal = BigDecimal(1)): TradeOrderPlacement =
    TradeOrderPlacement(uid, Enter(pos, cp, BigDecimal(price), volume), brokerParams, start.plusSeconds(hour * 3600))

  def mkExit(price: Double, hour: Long): TradeOrderPlacement =
    TradeOrderPlacement(uid, Exit(cp, BigDecimal(price)), brokerParams, start.plusSeconds(hour * 3600))

  def finalMark(price: Double, hour: Long): MarketMark =
    MarketMark(BigDecimal(price), start.plusSeconds(hour * 3600))

  "OrderStatsCollector" should {
    "calculate closed-trade statistics for a buy" in {
      val orders = List(
        mkEnter(Position.Buy, 100, 0),
        mkExit(110, 1)
      )
      val stats = OrderStatsCollector.collect(orders, settings = noCosts)

      stats.total mustBe 1
      stats.buys mustBe 1
      stats.sells mustBe 0
      stats.winCount mustBe 1
      stats.lossCount mustBe 0
      stats.winRate mustBe BigDecimal(1)
      stats.totalProfit mustBe BigDecimal(10)
      stats.expectancy mustBe BigDecimal(10)
      stats.payoffRatio mustBe None
      stats.profitFactor mustBe None
      stats.recoveryFactor mustBe None
      stats.completedTrades.head.openedAt mustBe start
      stats.completedTrades.head.closedAt mustBe start.plusSeconds(3600)
    }

    "calculate closed-trade statistics for a sell" in {
      val orders = List(
        mkEnter(Position.Sell, 100, 0),
        mkExit(90, 1)
      )
      val stats = OrderStatsCollector.collect(orders, settings = noCosts)

      stats.total mustBe 1
      stats.buys mustBe 0
      stats.sells mustBe 1
      stats.totalProfit mustBe BigDecimal(10)
    }

    "count only the closed side of a reversal and mark the new position to market" in {
      val orders = List(
        mkEnter(Position.Buy, 100, 0),
        mkEnter(Position.Sell, 90, 1)
      )
      val stats = OrderStatsCollector.collect(orders, Some(finalMark(80, 2)), noCosts)

      stats.total mustBe 1
      stats.buys mustBe 1
      stats.sells mustBe 0
      stats.lossCount mustBe 1
      stats.lossTotal mustBe -10.0
      stats.realizedProfit mustBe BigDecimal(-10)
      stats.unrealizedProfit mustBe BigDecimal(10)
      stats.totalProfit mustBe BigDecimal(0)
      stats.openPositions must have size 1
    }

    "exclude open positions from win rate even when they are profitable" in {
      val stats = OrderStatsCollector.collect(
        List(mkEnter(Position.Buy, 100, 0)),
        Some(finalMark(110, 1)),
        noCosts
      )

      stats.total mustBe 0
      stats.winCount mustBe 0
      stats.winRate mustBe BigDecimal(0)
      stats.unrealizedProfit mustBe BigDecimal(10)
      stats.equityCurve.last.realized mustBe false
    }

    "deduct spread, two-sided slippage and commission" in {
      val costs = RiskSettings(
        initialBalance = PosBigDecimal.unsafeFrom(BigDecimal(10000)),
        unitsPerLot = PosBigDecimal.unsafeFrom(BigDecimal(1)),
        transactionCosts = TransactionCosts(
          spreadPips = NonNegBigDecimal.unsafeFrom(BigDecimal(1)),
          slippagePipsPerSide = NonNegBigDecimal.unsafeFrom(BigDecimal("0.5")),
          commissionPerTrade = NonNegBigDecimal.unsafeFrom(BigDecimal(2))
        )
      )
      val stats = OrderStatsCollector.collect(
        List(
          mkEnter(Position.Buy, 1.1, 0, volume = BigDecimal(100000)),
          mkExit(1.101, 1)
        ),
        settings = costs
      )

      stats.preCostProfit.toDouble mustBe 100.0 +- 0.000001
      stats.totalCosts mustBe BigDecimal(22)
      stats.totalProfit.toDouble mustBe 78.0 +- 0.000001
    }

    "calculate expectancy, profit factor, drawdown and streaks from the equity curve" in {
      val orders = List(
        mkEnter(Position.Buy, 100, 0),
        mkExit(200, 1), // +100
        mkEnter(Position.Buy, 100, 2),
        mkExit(50, 3), // -50
        mkEnter(Position.Buy, 200, 4),
        mkExit(100, 5), // -100
        mkEnter(Position.Buy, 100, 6),
        mkExit(100, 7) // breakeven
      )
      val stats = OrderStatsCollector.collect(orders, settings = noCosts)

      stats.total mustBe 4
      stats.winCount mustBe 1
      stats.lossCount mustBe 2
      stats.breakevenCount mustBe 1
      stats.expectancy mustBe BigDecimal("-12.50000000")
      stats.averageWin mustBe BigDecimal(100)
      stats.averageLoss mustBe BigDecimal(75)
      stats.payoffRatio mustBe Some(BigDecimal("1.33333"))
      stats.profitFactor mustBe Some(BigDecimal("0.66667"))
      stats.maxDrawdown mustBe BigDecimal(150)
      stats.maxDrawdownPercent mustBe BigDecimal("13.63636364")
      stats.maxConsecutiveWins mustBe 1
      stats.maxConsecutiveLosses mustBe 2
    }

    "convert quote-currency profit when the account currency is the base" in {
      val usdCad = CurrencyPair(Currency.USD, Currency.CAD)
      val enter  = TradeOrderPlacement(
        uid,
        Enter(Position.Buy, usdCad, BigDecimal("1.24"), BigDecimal(10000)),
        brokerParams,
        start
      )
      val exit = TradeOrderPlacement(
        uid,
        Exit(usdCad, BigDecimal("1.25")),
        brokerParams,
        start.plusSeconds(3600)
      )
      val stats = OrderStatsCollector.collect(List(enter, exit), settings = noCosts)

      stats.preCostProfit mustBe BigDecimal(80)
    }

    "calculate annualized Sharpe and Sortino ratios from monthly equity returns" in {
      val february = 31L * 24
      val march    = (31L + 28L) * 24
      val stats    = OrderStatsCollector.collect(
        List(
          mkEnter(Position.Buy, 100, 0),
          mkExit(200, 1), // January +100
          mkEnter(Position.Buy, 100, february),
          mkExit(50, february + 1), // February -50
          mkEnter(Position.Buy, 100, march),
          mkExit(200, march + 1) // March +100
        ),
        settings = noCosts
      )

      stats.sharpeRatio mustBe 2.092 +- 0.001
      stats.sortinoRatio mustBe 6.5905 +- 0.001
    }

    "mark positions opened at the production fetch offset for H1 and M1 candles" in
      List(Interval.H1 -> 3700L, Interval.M1 -> 160L).foreach { case (interval, expectedMarkOffset) =>
        val bar  = PriceRange(100, 110, 90, 105, 1, start)
        val data = MarketTimeSeriesData(cp, interval, NonEmptyList.one(bar), "test")
        val open = TradeOrderPlacement(
          uid,
          Enter(Position.Buy, cp, BigDecimal(100), BigDecimal(1)),
          brokerParams,
          start.plusSeconds(100)
        )

        val finalMark = MarketMark(
          price = BigDecimal(data.prices.head.close),
          observedAt = data.latestTime.plus(data.interval.toDuration + 100.seconds)
        )

        val stats = OrderStatsCollector.collect(List(open), Some(finalMark), noCosts)

        stats.openPositions must have size 1
        stats.openPositions.head.markedAt mustBe start.plusSeconds(expectedMarkOffset)
        stats.unrealizedProfit mustBe BigDecimal(5)
      }

    "apply simultaneous portfolio closes as one equity event" in {
      val closedAt = start.plusSeconds(3600)
      val trades   = List(
        CompletedTrade(cp, Position.Buy, start, closedAt, 100, 110, 1, 100, 0, 100),
        CompletedTrade(CurrencyPair(Currency.USD, Currency.CAD), Position.Sell, start, closedAt, 100, 110, 1, -100, 0, -100)
      )

      val stats = OrderStats.fromTrades(trades, Nil, noCosts)

      stats.equityCurve must have size 1
      stats.equityCurve.head.equity mustBe BigDecimal(1000)
      stats.maxDrawdown mustBe BigDecimal(0)
      stats.completedTrades.map(_.returnPct).toSet mustBe Set(BigDecimal(10), BigDecimal(-10))
    }

    "track invalid duplicate and unmatched orders" in {
      val stats = OrderStatsCollector.collect(
        List(
          mkExit(100, 0),
          mkEnter(Position.Buy, 100, 1),
          mkEnter(Position.Buy, 101, 2),
          mkExit(110, 3)
        ),
        settings = noCosts
      )

      stats.invalidOrderCount mustBe 2
      stats.total mustBe 1
    }
  }
}
