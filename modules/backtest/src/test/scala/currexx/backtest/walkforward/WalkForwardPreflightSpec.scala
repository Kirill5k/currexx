package currexx.backtest.walkforward

import currexx.backtest.MarketDataProvider.{DateRange, HistoryCoverage}
import currexx.domain.market.{CurrencyPair, Interval}
import kirill5k.common.cats.test.IOWordSpec

import java.time.{Instant, YearMonth}

class WalkForwardPreflightSpec extends IOWordSpec:
  private val plan        = WalkForwardPlan.default.toOption.get
  private val monthlyBars = Iterator.iterate(YearMonth.of(2023, 7))(_.plusMonths(1)).take(36).map(_ -> 500L).toMap

  private def coverage(pair: String = "EURUSD", counts: Map[YearMonth, Long] = monthlyBars): HistoryCoverage =
    HistoryCoverage(
      CurrencyPair.fromUnsafe(pair),
      Interval.H1,
      Instant.parse("2023-07-02T21:00:00Z"),
      Instant.parse("2026-06-30T23:00:00Z"),
      counts
    )

  "WalkForwardPreflight.validate" should {
    "check all five expanding windows and every pair before any window runs" in {
      val history = List(coverage(), coverage("GBPUSD"))
      val ready   = WalkForwardPreflight.validate(history, plan).toOption.get

      ready.map(_.window) mustBe plan.windows
      ready.map(_.periods.size) mustBe List(10, 12, 14, 16, 18)
      ready.foreach { window =>
        window.periods.map(_.currencyPair).toSet mustBe Set("EURUSD", "GBPUSD")
        window.periods.foreach(_.priceBars mustBe 2000L)
        window.periods.count(_.stage == "selection") mustBe 2
        window.periods.count(_.stage == "forward test") mustBe 2
      }
      succeed
    }

    "report the oldest fold's 99-bar warm-up loss in every expanding window" in {
      val ready  = WalkForwardPreflight.validate(List(coverage()), plan).toOption.get
      val oldest = ready.map(_.periods.find(_.stage == "training fold 1").get)

      oldest must have size 5
      oldest.map(_.range).distinct mustBe List(plan.windows.head.trainingFolds.head)
      oldest.foreach { period =>
        period.priorBars mustBe 0L
        period.warmupBarsLost mustBe 99
        period.executableBars mustBe 1900L
      }
      succeed
    }

    "use earlier prices to warm later stages while excluding each simulator's priming bar" in {
      val ready  = WalkForwardPreflight.validate(List(coverage()), plan).toOption.get
      val warmed = ready.flatMap(_.periods.filterNot(_.stage == "training fold 1"))

      warmed.foreach { period =>
        period.priorBars must be >= 2000L
        period.warmupBarsLost mustBe 0
        period.executableBars mustBe period.priceBars - 1L
      }
      warmed.find(_.stage == "selection").get.priorBars mustBe 6000L
      warmed.find(_.stage == "forward test").get.priorBars mustBe 8000L
    }

    "attribute a missing month first needed by a later window to that window" in {
      val history = coverage(counts = monthlyBars - YearMonth.of(2026, 6))
      val failure = WalkForwardPreflight.validate(List(history), plan).swap.toOption.get

      failure.window mustBe plan.windows.last
      failure.getMessage must include("Window 5")
      failure.getMessage must include("forward test")
      failure.getMessage must include("2026-06")
    }

    "reject internal gaps even when the outer history bounds cover the whole plan" in {
      val history = coverage(counts = monthlyBars.updated(YearMonth.of(2024, 3), 0L))
      val failure = WalkForwardPreflight.validate(List(history), plan).swap.toOption.get

      history.firstBar mustBe coverage().firstBar
      history.lastBar mustBe coverage().lastBar
      failure.window mustBe plan.windows.head
      failure.getMessage must include("training fold 3")
      failure.getMessage must include("2024-03")
    }

    "require an execution bar after both price warm-up and simulator priming" in {
      val firstFoldMonths = (0L until 4L).map(YearMonth.of(2023, 7).plusMonths)
      val onlyOneWindow   = coverage(counts = monthlyBars ++ firstFoldMonths.map(_ -> 25L))
      val failure         = WalkForwardPreflight.validate(List(onlyOneWindow), plan).swap.toOption.get

      failure.window mustBe plan.windows.head
      failure.getMessage must include("Insufficient bars for warm-up, priming and execution")
      failure.getMessage must include("training fold 1")

      val oneExecution = onlyOneWindow.copy(perMonth = onlyOneWindow.perMonth.updated(YearMonth.of(2023, 7), 26L))
      val ready        = WalkForwardPreflight.validate(List(oneExecution), plan).toOption.get
      ready.head.periods.head.priceBars mustBe 101L
      ready.head.periods.head.warmupBarsLost mustBe 99
      ready.head.periods.head.executableBars mustBe 1L
    }

    "still require a next-bar execution when a selection period is already warmed" in {
      val start     = YearMonth.of(2024, 1)
      val shortPlan =
        WalkForwardPlan.expanding(DateRange(start, start.plusMonths(3)), initialTrainingMonths = 1, periodMonths = 1).toOption.get
      val history = coverage(counts = Map(start -> 101L, start.plusMonths(1) -> 1L, start.plusMonths(2) -> 10L)).copy(
        firstBar = Instant.parse("2024-01-01T00:00:00Z"),
        lastBar = Instant.parse("2024-03-31T23:00:00Z")
      )
      val failure = WalkForwardPreflight.validate(List(history), shortPlan).swap.toOption.get

      failure.window mustBe shortPlan.windows.head
      failure.getMessage must include("selection")
      failure.getMessage must include("Insufficient bars for warm-up, priming and execution")
    }
  }
