package currexx.backtest.optimizer.reporting

import currexx.backtest.TestStrategy
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import cats.effect.IO
import cats.syntax.all.*
import currexx.algorithms.EvaluationPhase
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation}
import kirill5k.common.cats.test.IOWordSpec

class RunDiagnosticsSpec extends IOWordSpec {
  private val first = TestStrategy(Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(10)), TradeStrategy(Nil, Nil))
  private val other = TestStrategy(Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(20)), TradeStrategy(Nil, Nil))

  "RunDiagnostics" should {
    "resolve equal discoveries independently of completion order and retain the earliest generation" in {
      def observe(order: List[TestStrategy]): IO[RunDiagnostics.Snapshot] = for {
        diagnostics <- RunDiagnostics.make[IO]
        _           <- order.traverse_(diagnostics.observed(_, EvaluationPhase.Search(4), List(0.5)))
        _           <- diagnostics.observed(other, EvaluationPhase.Search(0), List(0.5))
        _           <- diagnostics.observed(other, EvaluationPhase.Search(8), List(0.5))
        snapshot    <- diagnostics.snapshot
      } yield snapshot

      (observe(List(first, other)), observe(List(other, first))).tupled.asserting { case (forward, reverse) =>
        forward mustBe reverse
        forward.firstSeen mustBe Map(first -> 4, other -> 0)
        forward.bestSeen mustBe Some(RunDiagnostics.Discovery(other, 0.5, 0))
      }
    }

    "track candidates with identical indicators and different rules separately" in {
      val changed = first.copy(rules = TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil))
      (for {
        diagnostics <- RunDiagnostics.make[IO]
        _           <- diagnostics.observed(first, EvaluationPhase.Search(0), List(0.2))
        _           <- diagnostics.observed(changed, EvaluationPhase.Search(1), List(0.3))
        snapshot    <- diagnostics.snapshot
      } yield snapshot).asserting { snapshot =>
        snapshot.distinctSearchCandidates mustBe 2
        snapshot.firstSeen mustBe Map(first -> 0, changed -> 1)
        snapshot.bestSeen.map(_.candidate) mustBe Some(changed)
      }
    }

    "count concurrent simulations atomically and keep earlier snapshots unchanged by report work" in {
      val result = for {
        diagnostics <- RunDiagnostics.make[IO]
        _           <- List.fill(100)(()).parTraverse_ { _ =>
          diagnostics.pairStarted(RunDiagnostics.Stage.Search) *> diagnostics.pairCompleted(RunDiagnostics.Stage.Search)
        }
        before <- diagnostics.snapshot
        _      <- diagnostics.candidateRequested(RunDiagnostics.Stage.Reporting)
        _      <- diagnostics.foldStarted(RunDiagnostics.Stage.Reporting)
        _      <- diagnostics.pairStarted(RunDiagnostics.Stage.Reporting)
        after  <- diagnostics.snapshot
      } yield (before, after)

      result.asserting { case (before, after) =>
        before.workloads(RunDiagnostics.Stage.Search) mustBe RunDiagnostics.Workload(pairAttempts = 100, pairCompleted = 100)
        before.workloads(RunDiagnostics.Stage.Reporting) mustBe RunDiagnostics.Workload()
        after.workloads(RunDiagnostics.Stage.Search) mustBe before.workloads(RunDiagnostics.Stage.Search)
        after.workloads(RunDiagnostics.Stage.Reporting) mustBe RunDiagnostics.Workload(
          candidateRequests = 1,
          foldAttempts = 1,
          pairAttempts = 1
        )
      }
    }
  }
}
