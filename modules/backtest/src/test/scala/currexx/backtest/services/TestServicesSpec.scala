package currexx.backtest.services

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.backtest.{DataWindow, MarketMark, RiskSettings, TestSettings, TransactionCosts}
import currexx.backtest.types.given
import currexx.core.signal.{Signal, SignalDetector}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation}
import currexx.domain.user.UserId
import fs2.Stream
import kirill5k.common.cats.test.IOWordSpec

import java.time.Instant

class TestServicesSpec extends IOWordSpec {

  private val pair      = CurrencyPair.fromUnsafe("EURUSD")
  private val indicator = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, ValueTransformation.SMA(1))
  private val strategy  = TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil)
  private val settings  = TestSettings.make(pair, strategy, List(indicator))
  private val risk      = RiskSettings(transactionCosts = TransactionCosts(spreadPips = BigDecimal(0), slippagePipsPerSide = BigDecimal(0)))

  private val noSignals = new SignalDetector {
    override def detect(uid: UserId, data: MarketTimeSeriesData)(indicator: Indicator): Option[Signal] = None
  }

  private def bar(time: String, open: Double = 1.0, close: Double = 1.0, interval: Interval = Interval.M1): MarketTimeSeriesData =
    MarketTimeSeriesData(
      pair,
      interval,
      NonEmptyList.one(PriceRange(open, open.max(close), open.min(close), close, 100, Instant.parse(time))),
      "test"
    )

  private def closeTime(data: MarketTimeSeriesData): Instant = data.latestTime.plusSeconds(data.interval.toDuration.toSeconds).minusNanos(1)

  private def run(services: TestServices[IO], data: List[MarketTimeSeriesData], detector: SignalDetector): IO[Unit] =
    Stream.emits(data).covary[IO].through(services.processMarketData(detector)).compile.drain

  "TestServices equity accounting" should {

    "value open positions at real closes even when subsequent bars emit no signals" in {
      val prime    = bar("2026-01-15T10:00:00Z", interval = Interval.H1)
      val losing   = bar("2026-01-15T11:00:00Z", close = 0.9, interval = Interval.H1)
      val recovery = bar("2026-01-15T12:00:00Z", close = 1.1, interval = Interval.H1)
      val detector = new SignalDetector {
        override def detect(uid: UserId, data: MarketTimeSeriesData)(indicator: Indicator): Option[Signal] =
          if (data.latestTime == prime.latestTime) SignalDetector.pure.detect(uid, data)(indicator) else None
      }

      val result = for
        services <- TestServices.make[IO](settings)
        _        <- run(services, List(prime, losing, recovery), detector)
        orders   <- services.getAllOrders
        stats    <- services.getOrderStats(risk)
        reread   <- services.getAllOrders
      yield (orders, stats, reread)

      result.asserting { case (orders, stats, reread) =>
        orders must have size 1
        orders.head.order.price mustBe BigDecimal(1)
        // Accounting removes the fetch delay while the externally visible order history keeps its simulator timestamps.
        orders.head.time mustBe losing.latestTime.plusSeconds(100)
        reread mustBe orders
        stats.completedTrades.head.openedAt mustBe losing.latestTime
        stats.completedTrades.head.closedAt mustBe closeTime(recovery)
        stats.completedTrades.head.exitPrice mustBe BigDecimal("1.1")
        stats.equityCurve.find(_.time == closeTime(losing)).map(_.equity) mustBe Some(BigDecimal(9000))
        stats.equityCurve.find(_.time == closeTime(recovery)).map(_.equity) mustBe Some(BigDecimal(11000))
        stats.maxDrawdown mustBe BigDecimal(1000)
        stats.totalProfit mustBe BigDecimal(1000)
        stats.forcedClosureCount mustBe 1
        stats.dataWindow mustBe Some(DataWindow(losing.latestTime, closeTime(recovery)))
      }
    }

    "record every execution bar in an idle run and exclude the priming candle" in {
      val prime  = bar("2026-01-31T23:59:00Z")
      val first  = bar("2026-02-01T00:00:00Z", close = 0.8)
      val second = bar("2026-02-01T00:01:00Z", close = 1.2)

      val result = for
        services <- TestServices.make[IO](settings)
        _        <- run(services, List(prime, first, second), noSignals)
        stats    <- services.getOrderStats(risk)
      yield stats

      result.asserting { stats =>
        stats.total mustBe 0
        stats.dataWindow mustBe Some(DataWindow(first.latestTime, closeTime(second)))
        stats.profitByMonth mustBe Map("2026-02" -> BigDecimal(0))
        (stats.equityCurve.map(_.time) must contain).allOf(closeTime(first), closeTime(second))
        stats.equityCurve.map(_.equity).distinct mustBe List(BigDecimal(10000))
        stats.equityCurve.exists(_.time.isBefore(first.latestTime)) mustBe false
      }
    }

    "keep the last candle's profit in its month despite the simulator fetch delay" in {
      val prime = bar("2026-01-31T22:00:00Z", interval = Interval.H1)
      val last  = bar("2026-01-31T23:00:00Z", close = 1.1, interval = Interval.H1)

      val result = for
        services <- TestServices.make[IO](settings)
        _        <- run(services, List(prime, last), SignalDetector.pure)
        orders   <- services.getAllOrders
        stats    <- services.getOrderStats(risk)
      yield (orders, stats)

      result.asserting { case (orders, stats) =>
        orders.head.time mustBe Instant.parse("2026-01-31T23:01:40Z")
        stats.completedTrades.head.openedAt mustBe last.latestTime
        stats.completedTrades.head.closedAt mustBe Instant.parse("2026-01-31T23:59:59.999999999Z")
        stats.profitByMonth mustBe Map("2026-01" -> BigDecimal(1000))
        stats.invalidOrderCount mustBe 0
        stats.dataWindow mustBe Some(DataWindow(last.latestTime, closeTime(last)))
      }
    }

    "clear prior marks and orders when a services instance is reused" in {
      val priorPrime = bar("2026-01-01T00:00:00Z", interval = Interval.H1)
      val priorBar   = bar("2026-01-01T01:00:00Z", close = 0.9, interval = Interval.H1)
      val nextPrime  = bar("2026-03-01T00:00:00Z")
      val nextBar    = bar("2026-03-01T00:01:00Z", close = 1.2)

      val result = for
        services   <- TestServices.make[IO](settings)
        _          <- run(services, List(priorPrime, priorBar), SignalDetector.pure)
        _          <- services.reset(settings)
        resetStats <- services.getOrderStats(risk)
        _          <- run(services, List(nextPrime, nextBar), noSignals)
        stats      <- services.getOrderStats(risk)
      yield (resetStats, stats)

      result.asserting { case (resetStats, stats) =>
        resetStats.dataWindow mustBe None
        resetStats.completedTrades mustBe empty
        resetStats.equityCurve mustBe empty
        stats.completedTrades mustBe empty
        stats.totalProfit mustBe BigDecimal(0)
        stats.maxDrawdown mustBe BigDecimal(0)
        stats.profitByMonth mustBe Map("2026-03" -> BigDecimal(0))
        stats.dataWindow mustBe Some(DataWindow(nextBar.latestTime, closeTime(nextBar)))
        stats.equityCurve.exists(_.time.isBefore(nextBar.latestTime)) mustBe false
      }
    }

    "leave accounting empty when there is only a priming candle" in {
      val result = for
        services <- TestServices.make[IO](settings)
        _        <- run(services, List(bar("2026-01-01T00:00:00Z")), SignalDetector.pure)
        stats    <- services.getOrderStats(risk)
      yield stats

      result.asserting { stats =>
        stats.dataWindow mustBe None
        stats.total mustBe 0
        stats.equityCurve mustBe empty
        stats.profitByMonth mustBe empty
      }
    }

    "raise conflicting closes for a repeated candle through the stats effect" in {
      val prime   = bar("2026-02-01T00:00:00Z")
      val first   = bar("2026-02-01T00:01:00Z", close = 0.8)
      val revised = bar("2026-02-01T00:01:00Z", close = 1.2)

      val result = for
        services <- TestServices.make[IO](settings)
        _        <- run(services, List(prime, first, revised), noSignals)
        effect   <- IO(services.getOrderStats(risk))
        outcome  <- effect.attempt
      yield outcome

      result.asserting {
        case Left(error: IllegalArgumentException) =>
          error.getMessage.toLowerCase must include("conflict")
        case other => fail(s"Expected conflicting market marks to fail accounting, got $other")
      }
    }
  }

  "ApplicationState execution accounting" should {

    "separate fill and close times from a fetch delay longer than the candle" in {
      val data   = bar("2026-01-31T23:59:00Z", close = 0.9)
      val result = for
        state     <- ApplicationState.make[IO](settings)
        _         <- state.prepareExecution(data)
        execution <- state.dataRef.get
        clock     <- state.clockRef.get
        marks     <- state.marketMarksRef.get
        window    <- state.dataWindowRef.get
      yield (execution, clock, marks, window, clock.map(state.accountingTime))

      result.asserting { case (execution, clock, marks, window, fillTime) =>
        execution.map(_.prices.head.close) mustBe Some(1.0)
        clock mustBe Some(Instant.parse("2026-02-01T00:00:40Z"))
        fillTime mustBe Some(data.latestTime)
        marks mustBe List(MarketMark(BigDecimal("0.9"), closeTime(data)))
        window mustBe Some(DataWindow(data.latestTime, closeTime(data)))
      }
    }
  }
}
