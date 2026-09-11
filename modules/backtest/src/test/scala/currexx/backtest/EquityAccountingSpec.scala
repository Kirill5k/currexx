package currexx.backtest

import currexx.backtest.types.given
import currexx.clients.broker.BrokerParameters
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{Currency, CurrencyPair}
import currexx.domain.market.TradeOrder.{Enter, Exit, Position}
import currexx.domain.user.UserId
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class EquityAccountingSpec extends AnyWordSpec with Matchers {

  private val uid          = UserId("equity-accounting")
  private val brokerParams = BrokerParameters.Oanda("key", true, "account")
  private val eurUsd       = CurrencyPair(Currency.EUR, Currency.USD)
  private val start        = Instant.parse("2025-01-01T00:00:00Z")
  private val noCosts      = RiskSettings(
    initialBalance = BigDecimal(1000),
    unitsPerLot = BigDecimal(1),
    transactionCosts = TransactionCosts(
      spreadPips = BigDecimal(0),
      slippagePipsPerSide = BigDecimal(0),
      commissionPerTrade = BigDecimal(0)
    )
  )

  private def hour(value: Long): Instant = start.plusSeconds(value * 3600)

  private def enter(
      position: Position,
      price: BigDecimal,
      time: Instant,
      pair: CurrencyPair = eurUsd
  ): TradeOrderPlacement =
    TradeOrderPlacement(uid, Enter(position, pair, price, BigDecimal(1)), brokerParams, time)

  private def exit(price: BigDecimal, time: Instant, pair: CurrencyPair = eurUsd): TradeOrderPlacement =
    TradeOrderPlacement(uid, Exit(pair, price), brokerParams, time)

  private def mark(price: BigDecimal, time: Instant): MarketMark = MarketMark(price, time)

  private def equityAt(stats: OrderStats, time: Instant): Option[BigDecimal] =
    stats.equityCurve.find(_.time == time).map(_.equity)

  private def collectMarked(
      orders: List[TradeOrderPlacement],
      marketMarks: List[MarketMark],
      settings: RiskSettings = noCosts,
      dataWindow: Option[DataWindow] = None
  ): OrderStats =
    OrderStatsCollector.collect(orders, marketMarks, settings, dataWindow).fold(error => fail(error.getMessage, error), identity)

  "Marked equity accounting" should {
    "reject conflicting prices at an intermediate or terminal timestamp regardless of input order" in {
      val orders = List(enter(Position.Buy, 100, hour(0)))
      val marks  = List(mark(100, hour(0)), mark(90, hour(1)), mark(110, hour(2)))

      List(mark(95, hour(1)), mark(120, hour(2))).foreach { conflicting =>
        (conflicting :: marks).permutations.foreach { observations =>
          OrderStatsCollector.collect(orders, observations, noCosts).isLeft mustBe true
        }
      }
    }

    "accept repeated identical marks without changing valuations or terminal profit" in {
      val orders   = List(enter(Position.Buy, 100, hour(0)))
      val opening  = mark(100, hour(0))
      val low      = mark(90, hour(1))
      val terminal = mark(110, hour(2))
      val expected = collectMarked(orders, List(opening, low, terminal))

      List(opening, low, terminal, low, terminal).permutations.foreach { observations =>
        collectMarked(orders, observations) mustBe expected
      }

      expected.forcedClosureCount mustBe 1
      expected.totalProfit mustBe BigDecimal(10)
      expected.maxDrawdown mustBe BigDecimal(10)
    }

    "reject missing observations when orders or a data window describe an actual run" in {
      val orders = List(enter(Position.Buy, 100, hour(0)), exit(110, hour(2)))
      val window = Some(DataWindow(hour(0), hour(2)))

      OrderStatsCollector.collect(orders, Nil, noCosts).isLeft mustBe true
      OrderStatsCollector.collect(Nil, Nil, noCosts, window).isLeft mustBe true
      OrderStatsCollector.collect(orders, Nil, noCosts, window).isLeft mustBe true
    }

    "reject an open position entered after the latest supplied mark" in {
      val orders = List(enter(Position.Buy, 100, hour(2)))
      val marks  = List(mark(100, hour(0)), mark(110, hour(1)))

      OrderStatsCollector.collect(orders, marks, noCosts).isLeft mustBe true
    }

    "return a validation error when forced liquidation needs a missing account conversion rate" in {
      val pair   = CurrencyPair.fromUnsafe("GBPJPY")
      val orders = List(enter(Position.Buy, 150, hour(0), pair))
      val marks  = List(mark(150, hour(0)), mark(151, hour(1)))

      OrderStatsCollector.collect(orders, marks, noCosts).isLeft mustBe true
    }

    "allow an empty result before any orders or market data have been observed" in {
      val stats = collectMarked(Nil, Nil)

      stats.total mustBe 0
      stats.totalProfit mustBe BigDecimal(0)
      stats.equityCurve mustBe empty
      stats.profitByMonth mustBe empty
    }

    "make trade-only risk an explicit choice when the observed path is unavailable" in {
      val orders = List(enter(Position.Buy, 100, hour(0)), exit(110, hour(2)))
      val marked = collectMarked(orders, List(mark(100, hour(0)), mark(90, hour(1)), mark(110, hour(2))))
      val sparse = OrderStatsCollector
        .collectTradeOnly(orders, settings = noCosts)
        .fold(error => fail(error.getMessage, error), identity)

      sparse.completedTrades mustBe marked.completedTrades
      sparse.totalProfit mustBe marked.totalProfit
      sparse.maxDrawdown mustBe BigDecimal(0)
      marked.maxDrawdown mustBe BigDecimal(10)
      marked.equityCurve.last.equity mustBe sparse.equityCurve.last.equity
    }

    "measure a winning buy's loss while the position is still open" in {
      val stats = collectMarked(
        List(enter(Position.Buy, 100, hour(0)), exit(110, hour(2))),
        settings = noCosts,
        marketMarks = List(mark(100, hour(0)), mark(90, hour(1)), mark(110, hour(2)))
      )

      stats.totalProfit mustBe BigDecimal(10)
      stats.winCount mustBe 1
      equityAt(stats, hour(1)) mustBe Some(BigDecimal(990))
      stats.maxDrawdown mustBe BigDecimal(10)
      stats.maxDrawdownPercent mustBe BigDecimal(1)
      stats.recoveryFactor mustBe Some(BigDecimal(1))
      stats.equityCurve.last.equity mustBe noCosts.initialBalance.value + stats.totalProfit
    }

    "measure an adverse price increase during a winning short" in {
      val stats = collectMarked(
        List(enter(Position.Sell, 100, hour(0))),
        settings = noCosts,
        marketMarks = List(mark(100, hour(0)), mark(120, hour(1)), mark(90, hour(2)))
      )

      equityAt(stats, hour(1)) mustBe Some(BigDecimal(980))
      stats.maxDrawdown mustBe BigDecimal(20)
      stats.maxDrawdownPercent mustBe BigDecimal(2)
      stats.totalProfit mustBe BigDecimal(10)
      stats.forcedClosureCount mustBe 1
      stats.equityCurve.last.equity mustBe noCosts.initialBalance.value + stats.totalProfit
    }

    "measure a retreat from an unrealised peak even when equity stays above the initial balance" in {
      val stats = collectMarked(
        List(enter(Position.Buy, 100, hour(0)), exit(160, hour(3))),
        settings = noCosts,
        marketMarks = List(mark(100, hour(0)), mark(150, hour(1)), mark(120, hour(2)), mark(160, hour(3)))
      )

      equityAt(stats, hour(1)) mustBe Some(BigDecimal(1050))
      equityAt(stats, hour(2)) mustBe Some(BigDecimal(1020))
      stats.maxDrawdown mustBe BigDecimal(30)
      stats.maxDrawdownPercent mustBe BigDecimal("2.85714286")
      stats.totalProfit mustBe BigDecimal(60)
    }

    "attribute an open position's gains and losses to each month in which equity changes" in {
      val openedAt = Instant.parse("2025-01-10T00:00:00Z")
      val january  = Instant.parse("2025-01-31T23:00:00Z")
      val february = Instant.parse("2025-02-28T23:00:00Z")
      val closedAt = Instant.parse("2025-03-15T00:00:00Z")
      val stats    = collectMarked(
        List(enter(Position.Buy, 100, openedAt), exit(110, closedAt)),
        settings = noCosts,
        dataWindow = Some(DataWindow(start, Instant.parse("2025-03-31T23:00:00Z"))),
        marketMarks = List(mark(100, openedAt), mark(120, january), mark(90, february), mark(110, closedAt))
      )

      stats.profitByMonth mustBe Map("2025-01" -> BigDecimal(20), "2025-02" -> BigDecimal(-30), "2025-03" -> BigDecimal(20))
      stats.profitByMonth.values.sum mustBe stats.totalProfit
      stats.sortinoRatio mustBe a[RiskRatio.Defined]
      stats.completedTrades.head.netProfit mustBe BigDecimal(10)
    }

    "include flat months before, between and after trades in calendar returns" in {
      val openedAt = Instant.parse("2025-02-15T00:00:00Z")
      val closedAt = Instant.parse("2025-02-16T00:00:00Z")
      val end      = Instant.parse("2025-04-30T23:00:00Z")
      val stats    = collectMarked(
        List(enter(Position.Buy, 100, openedAt), exit(200, closedAt)),
        settings = noCosts,
        dataWindow = Some(DataWindow(start, end)),
        marketMarks = List(mark(100, start), mark(100, openedAt), mark(200, closedAt), mark(200, end))
      )

      stats.profitByMonth mustBe Map(
        "2025-01" -> BigDecimal(0),
        "2025-02" -> BigDecimal(100),
        "2025-03" -> BigDecimal(0),
        "2025-04" -> BigDecimal(0)
      )
      stats.sharpeRatio.toOption.get mustBe math.sqrt(3) +- 0.000001
      stats.sortinoRatio mustBe RiskRatio.ZeroDeviation
    }

    "record every evaluation month when a strategy never opens a position" in {
      val end   = Instant.parse("2025-03-31T23:00:00Z")
      val stats = collectMarked(
        Nil,
        settings = noCosts,
        dataWindow = Some(DataWindow(start, end)),
        marketMarks = List(mark(100, start), mark(110, end))
      )

      stats.profitByMonth mustBe Map("2025-01" -> BigDecimal(0), "2025-02" -> BigDecimal(0), "2025-03" -> BigDecimal(0))
      stats.sharpeRatio mustBe RiskRatio.ZeroDeviation
      stats.sortinoRatio mustBe RiskRatio.ZeroDeviation
      stats.maxDrawdown mustBe BigDecimal(0)
      stats.totalProfit mustBe BigDecimal(0)
    }

    "retain simultaneous unrealised losses when pooling accounts" in {
      def member(low: BigDecimal): OrderStats = collectMarked(
        List(enter(Position.Buy, 100, hour(0))),
        settings = noCosts,
        marketMarks = List(mark(100, hour(0)), mark(low, hour(1)), mark(110, hour(2)))
      )

      val pooled = OrderStats.combine(List(member(90), member(80)))

      pooled.initialBalance mustBe BigDecimal(2000)
      equityAt(pooled, hour(1)) mustBe Some(BigDecimal(1970))
      pooled.maxDrawdown mustBe BigDecimal(30)
      pooled.maxDrawdownPercent mustBe BigDecimal("1.5")
      pooled.totalProfit mustBe BigDecimal(20)
      pooled.equityCurve.last.equity mustBe pooled.initialBalance + pooled.totalProfit
    }

    "apply simultaneous gains and losses as one portfolio valuation" in {
      def member(intermediate: BigDecimal): OrderStats = collectMarked(
        List(enter(Position.Buy, 100, hour(0)), exit(100, hour(2))),
        settings = noCosts,
        marketMarks = List(mark(100, hour(0)), mark(intermediate, hour(1)), mark(100, hour(2)))
      )

      val gain = member(150)
      val loss = member(50)

      List(List(gain, loss), List(loss, gain)).foreach { members =>
        val pooled = OrderStats.combine(members)

        pooled.maxDrawdown mustBe BigDecimal(0)
        pooled.equityCurve.map(_.equity).toSet mustBe Set(BigDecimal(2000))
        pooled.equityCurve.count(_.time == hour(1)) mustBe 1
      }
    }

    "carry each account's latest equity forward between asynchronous marks" in {
      val first = collectMarked(
        List(enter(Position.Buy, 100, hour(0))),
        settings = noCosts,
        marketMarks = List(mark(100, hour(0)), mark(90, hour(1)), mark(110, hour(3)))
      )
      val second = collectMarked(
        List(enter(Position.Buy, 100, hour(0))),
        settings = noCosts,
        marketMarks = List(mark(100, hour(0)), mark(80, hour(2)), mark(110, hour(4)))
      )

      val pooled = OrderStats.combine(List(first, second))

      equityAt(pooled, hour(1)) mustBe Some(BigDecimal(1990))
      equityAt(pooled, hour(2)) mustBe Some(BigDecimal(1970))
      equityAt(pooled, hour(3)) mustBe Some(BigDecimal(1990))
      equityAt(pooled, hour(4)) mustBe Some(BigDecimal(2020))
      pooled.maxDrawdown mustBe BigDecimal(30)
      pooled.maxDrawdownPercent mustBe BigDecimal("1.5")
    }

    "reserve liquidation costs while open and charge each reversed trade only once" in {
      val settings = noCosts.copy(
        unitsPerLot = BigDecimal(100000),
        transactionCosts = TransactionCosts(
          spreadPips = BigDecimal(1),
          slippagePipsPerSide = BigDecimal("0.5"),
          commissionPerTrade = BigDecimal(2)
        )
      )
      val stats = collectMarked(
        List(
          enter(Position.Buy, BigDecimal("1.100"), hour(0)),
          enter(Position.Sell, BigDecimal("1.101"), hour(1)),
          exit(BigDecimal("1.100"), hour(3))
        ),
        settings = settings,
        marketMarks = List(
          mark(BigDecimal("1.100"), hour(0)),
          mark(BigDecimal("1.101"), hour(1)),
          mark(BigDecimal("1.102"), hour(2)),
          mark(BigDecimal("1.100"), hour(3))
        )
      )

      equityAt(stats, hour(0)) mustBe Some(BigDecimal(978))
      equityAt(stats, hour(1)) mustBe Some(BigDecimal(1056))
      equityAt(stats, hour(2)) mustBe Some(BigDecimal(956))
      stats.total mustBe 2
      stats.preCostProfit mustBe BigDecimal(200)
      stats.totalCosts mustBe BigDecimal(44)
      stats.totalProfit mustBe BigDecimal(156)
      stats.equityCurve.last.equity mustBe BigDecimal(1156)
      stats.profitByMonth.values.sum mustBe stats.totalProfit
    }

    "convert cross-pair profit and costs using the configured account rate at every mark" in {
      val pair     = CurrencyPair.fromUnsafe("GBPJPY")
      val settings = noCosts.copy(
        unitsPerLot = BigDecimal(100),
        quoteToAccountRates = Map(pair.quote -> BigDecimal("0.01")),
        transactionCosts = TransactionCosts(
          spreadPips = BigDecimal(1),
          slippagePipsPerSide = BigDecimal("0.5"),
          commissionPerTrade = BigDecimal("0.5")
        )
      )
      val stats = collectMarked(
        List(enter(Position.Buy, 150, hour(0), pair)),
        List(mark(150, hour(0)), mark(140, hour(1)), mark(160, hour(2))),
        settings
      )

      // Two JPY pips cost JPY 2 for 100 units, or USD 0.02, plus USD 0.50 commission.
      equityAt(stats, hour(0)) mustBe Some(BigDecimal("999.48"))
      equityAt(stats, hour(1)) mustBe Some(BigDecimal("989.48"))
      stats.preCostProfit mustBe BigDecimal(10)
      stats.totalCosts mustBe BigDecimal("0.52")
      stats.totalProfit mustBe BigDecimal("9.48")
      stats.maxDrawdown mustBe BigDecimal("10.52")
      stats.forcedClosureCount mustBe 1
      stats.equityCurve.last.equity mustBe BigDecimal("1009.48")
      stats.profitByMonth.values.sum mustBe stats.totalProfit
    }

    "return a validation error for non-positive prices used to convert into the pair's base currency" in {
      val pair = CurrencyPair(Currency.USD, Currency.CAD)

      List(BigDecimal(0), BigDecimal(-1)).foreach { invalidPrice =>
        val markedOrders = List(enter(Position.Buy, 2, hour(0), pair), exit(4, hour(2), pair))
        val marks        = List(mark(2, hour(0)), mark(invalidPrice, hour(1)), mark(4, hour(2)))
        val sparseOrders = List(enter(Position.Buy, 2, hour(0), pair), exit(invalidPrice, hour(1), pair))

        OrderStatsCollector.collect(markedOrders, marks, noCosts).isLeft mustBe true
        OrderStatsCollector.collectTradeOnly(sparseOrders, settings = noCosts).isLeft mustBe true
      }
    }

    "convert open profit using each mark when the account currency is the pair's base" in {
      val pair  = CurrencyPair(Currency.USD, Currency.CAD)
      val stats = collectMarked(
        List(enter(Position.Buy, 2, hour(0), pair), exit(4, hour(2), pair)),
        settings = noCosts.copy(unitsPerLot = BigDecimal(100)),
        marketMarks = List(mark(2, hour(0)), mark(1, hour(1)), mark(4, hour(2)))
      )

      // At the low mark, CAD -100 converts to USD -100; at exit, CAD +200 converts to USD +50.
      equityAt(stats, hour(1)) mustBe Some(BigDecimal(900))
      stats.maxDrawdown mustBe BigDecimal(100)
      stats.maxDrawdownPercent mustBe BigDecimal(10)
      stats.totalProfit mustBe BigDecimal(50)
      stats.equityCurve.last.equity mustBe BigDecimal(1050)
    }
  }
}
