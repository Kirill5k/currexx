package currexx.backtest.walkforward

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.apply.*
import currexx.backtest.{MarketDataProvider, TestSettings, TestStrategy}
import currexx.backtest.MarketDataProvider.{Dataset, DateRange}
import currexx.backtest.services.{PeriodSimulation, TestServices}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import java.time.{Instant, YearMonth}

class ForwardEvaluatorSpec extends IOWordSpec:
  private val period = Some(DateRange(YearMonth.of(2024, 1), YearMonth.of(2024, 3)))
  private val single = Dataset("eur-usd-1h-walk-forward-full.csv", period)
  private val split  = Dataset(NonEmptyList.of("eur-usd-1h-position-first.csv", "eur-usd-1h-position-second.csv"), period)
  private val long   = TestStrategy(
    Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1)),
    TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil)
  )
  private val short = long.copy(rules = TradeStrategy(List(Rule(TradeAction.OpenShort, Rule.Condition.NoPosition)), Nil))

  "ForwardEvaluator" should {
    "produce identical continuous trades across file boundaries, with one terminal liquidation" in {
      def simulate(dataset: Dataset) = for
        data     <- MarketDataProvider.read[IO](dataset).compile.toList
        services <- TestServices.make[IO](TestSettings.make(dataset.currencyPair, long.rules, List(long.indicator)))
        stats    <- PeriodSimulation.run(services, data)
      yield stats

      (simulate(single), simulate(split)).tupled.asserting { case (whole, parts) =>
        parts mustBe whole
        parts.total mustBe 1
        parts.forcedClosureCount mustBe 1
        val trade = parts.completedTrades.head
        trade.openedAt.isBefore(Instant.parse("2024-02-03T02:00:00Z")) mustBe true
        trade.closedAt.isAfter(Instant.parse("2024-02-03T02:00:00Z")) mustBe true
      }
    }

    "use identical costs and execution coverage for the frozen strategy and base" in
      ForwardEvaluator.make[IO](2).evaluate(short, long, List(split)).asserting { result =>
        result.candidate.initialBalance mustBe result.base.initialBalance
        result.candidate.closedTrades mustBe 1
        result.base.closedTrades mustBe 1
        result.candidate.forcedClosures mustBe result.base.forcedClosures
        result.candidate.costs mustBe result.base.costs
        result.candidate.netProfit must be < result.base.netProfit
        val coverage = result.coverage.head
        coverage.firstWindow mustBe Instant.parse("2024-01-31T23:00:00Z")
        coverage.firstExecution mustBe Instant.parse("2024-02-01T00:00:00Z")
        coverage.lastMark mustBe Instant.parse("2024-02-05T23:59:59.999999999Z")
        coverage.warmupBarsLost mustBe 99
      }

    "report zero paired differences when the base is retained and exclude prior prices from trading" in {
      val february = split.copy(range = Some(DateRange(YearMonth.of(2024, 2), YearMonth.of(2024, 3))))
      ForwardEvaluator.make[IO](1).evaluate(long, long, List(february)).asserting { result =>
        result.candidate mustBe result.base
        result.netDifference mustBe BigDecimal(0)
        result.coverage.head.firstExecution mustBe Instant.parse("2024-02-01T01:00:00Z")
        result.coverage.head.warmupBarsLost mustBe 0
      }
    }
  }
