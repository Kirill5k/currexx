package currexx.backtest.walkforward

import currexx.backtest.MarketDataProvider.DateRange
import kirill5k.common.cats.test.IOWordSpec

import java.time.YearMonth

class WalkForwardPlanSpec extends IOWordSpec:
  private def range(from: String, until: String): DateRange = DateRange(YearMonth.parse(from), YearMonth.parse(until))

  "WalkForwardPlan" should {
    "produce five chronological expanding windows without sharing test months" in {
      val windows = WalkForwardPlan.default.toOption.get.windows
      windows.map(_.training) mustBe List("2024-07", "2024-11", "2025-03", "2025-07", "2025-11").map(range("2023-07", _))
      windows.map(_.trainingFolds.size) mustBe List(3, 4, 5, 6, 7)
      windows.map(_.selection) mustBe List(
        range("2024-07", "2024-11"),
        range("2024-11", "2025-03"),
        range("2025-03", "2025-07"),
        range("2025-07", "2025-11"),
        range("2025-11", "2026-03")
      )
      windows.map(_.test) mustBe List(
        range("2024-11", "2025-03"),
        range("2025-03", "2025-07"),
        range("2025-07", "2025-11"),
        range("2025-11", "2026-03"),
        range("2026-03", "2026-07")
      )
    }

    "omit an incomplete trailing test without truncating it" in {
      val result = WalkForwardPlan.expanding(range("2023-07", "2026-06")).toOption.get
      result.windows mustBe WalkForwardPlan.default.toOption.get.windows.take(4)
    }

    "reject insufficient history, invalid fold lengths, empty folds and overlapping stages" in {
      WalkForwardPlan.expanding(range("2023-07", "2025-02")).isLeft mustBe true
      WalkForwardPlan.expanding(range("2023-07", "2026-07"), periodMonths = 0).isLeft mustBe true
      WalkForwardPlan.expanding(range("2023-07", "2026-07"), initialTrainingMonths = 13).isLeft mustBe true
      val first = WalkForwardPlan.default.toOption.get.windows.head
      WalkForwardPlan.validate(List(first.copy(trainingFolds = Nil))).isLeft mustBe true
      WalkForwardPlan.validate(List(first.copy(trainingFolds = Nil), WalkForwardPlan.default.toOption.get.windows(1))).isLeft mustBe true
      WalkForwardPlan.validate(List(first.copy(selection = first.training))).isLeft mustBe true
      WalkForwardPlan.validate(List(first, first.copy(index = 2))).isLeft mustBe true
    }

    "derive stable, distinct seeds from dates rather than traversal order or window numbering" in {
      val windows = WalkForwardPlan.default.toOption.get.windows
      val seeds   = windows.map(_.seed(42L))
      WalkForwardWindow.seedVersion mustBe "walk-forward-v2"
      seeds.head mustBe -6814895312625154181L
      seeds.distinct.size mustBe windows.size
      windows.reverse.map(_.seed(42L)).reverse mustBe seeds
      windows.head.copy(index = 99).seed(42L) mustBe seeds.head
      windows.head.seed(43L) must not be seeds.head
    }
  }
