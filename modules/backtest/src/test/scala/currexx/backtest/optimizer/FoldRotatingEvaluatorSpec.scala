package currexx.backtest.optimizer

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import currexx.algorithms.EvaluationPhase
import currexx.backtest.{OrderStats, TestStrategy}
import currexx.backtest.optimizer.reporting.RunDiagnostics
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation}
import kirill5k.common.cats.test.IOWordSpec

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

class FoldRotatingEvaluatorSpec extends IOWordSpec {

  private val indicator = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(10))
  private val other     = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(20))
  private val scores    = List(0.2, 0.5, 0.8)

  private def scoring(onScore: List[OrderStats] => Unit = _ => ()): ScoringFunction = new ScoringFunction {
    override def score(stats: List[OrderStats]): Double = {
      onScore(stats)
      stats.map(_.totalProfit.toDouble).sum
    }

    override def violations(stats: List[OrderStats]): List[ScoringFunction.Violation] = Nil
  }

  private def stats(score: Double): List[OrderStats] = List(OrderStats(totalProfit = BigDecimal(score)))

  "FoldRotatingEvaluator.cached" should {

    "preserve the rotating search and full rescore fitness from unequal fold scores" in {
      val backtests = scores.map(score => (_: Indicator) => IO.pure(stats(score)))
      val phases    = List.tabulate(4)(EvaluationPhase.Search(_)) :+ EvaluationPhase.Rescore
      val result    = for
        evaluator <- FoldRotatingEvaluator.cached[IO](backtests, scoring())
        evaluated <- phases.traverse(evaluator.evaluateIndividual(indicator, _))
      yield evaluated

      result.asserting { evaluated =>
        evaluated.map(_._1).distinct mustBe List(indicator)
        val expected = List(
          math.sqrt(0.51 * 0.81) - 0.01,
          math.sqrt(0.21 * 0.81) - 0.01,
          math.sqrt(0.21 * 0.51) - 0.01,
          math.sqrt(0.51 * 0.81) - 0.01,
          math.pow(0.21 * 0.51 * 0.81, 1.0 / 3.0) - 0.01
        )
        evaluated.map(_._2.value).zip(expected).foreach { case (actual, expectedScore) =>
          actual mustBe expectedScore +- 1e-12
        }
        succeed
      }
    }

    "backtest and score each fold once per distinct candidate across all phases" in {
      val scoreCalls = new AtomicInteger(0)
      val result     = for
        calls <- Ref.of[IO, List[(Indicator, Int)]](Nil)
        backtests = scores.zipWithIndex.map { case (score, fold) =>
          (candidate: Indicator) => calls.update(_ :+ (candidate -> fold)).as(stats(score))
        }
        diagnostics <- RunDiagnostics.make[IO]
        evaluator   <- FoldRotatingEvaluator
          .cached[IO](backtests, scoring { _ => scoreCalls.incrementAndGet(); () }, observer = Some(diagnostics.evaluatorObserver))
        _ <- List(indicator, other, indicator, other).traverse_ { candidate =>
          List(EvaluationPhase.Search(0), EvaluationPhase.Search(2), EvaluationPhase.Rescore)
            .traverse_(evaluator.evaluateIndividual(candidate, _))
        }
        recorded <- calls.get
        snapshot <- diagnostics.snapshot
      yield (recorded, snapshot)

      result.asserting { case (recorded, snapshot) =>
        recorded mustBe List(indicator, other).flatMap(candidate => scores.indices.map(candidate -> _))
        scoreCalls.get() mustBe 6
        snapshot.searchRequests mustBe 8L
        snapshot.rescoreRequests mustBe 4L
        snapshot.successfulSearchRequests mustBe 8L
        snapshot.computationAttempts mustBe 2L
        snapshot.completedComputations mustBe 2L
        snapshot.uniqueComputedCandidates mustBe 2
        snapshot.distinctSearchCandidates mustBe 2
        snapshot.cacheReuses mustBe 10L
        snapshot.firstSeen mustBe Map(indicator -> 0, other -> 0)
        snapshot.workloads(RunDiagnostics.Stage.Search) mustBe RunDiagnostics.Workload(
          candidateRequests = 12,
          foldAttempts = 6,
          foldCompleted = 6
        )
      }
    }

    "share an in-flight backtest between concurrent callers for the same candidate" in {
      val result = for
        started <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        calls   <- Ref.of[IO, Int](0)
        backtest = (_: Indicator) => calls.update(_ + 1) >> started.complete(()) >> release.get.as(stats(0.5))
        diagnostics <- RunDiagnostics.make[IO]
        evaluator   <- FoldRotatingEvaluator.cached[IO](List(backtest), scoring(), observer = Some(diagnostics.evaluatorObserver))
        first       <- evaluator.evaluateIndividual(indicator, EvaluationPhase.Search(0)).start
        _           <- started.get
        second      <- evaluator.evaluateIndividual(indicator, EvaluationPhase.Rescore).start
        _           <- IO.cede
        _           <- release.complete(())
        searched    <- first.joinWithNever
        rescored    <- second.joinWithNever
        count       <- calls.get
        snapshot    <- diagnostics.snapshot
      yield (searched, rescored, count, snapshot)

      result.asserting { case (searched, rescored, count, snapshot) =>
        searched mustBe rescored
        searched._2.value mustBe 0.5
        count mustBe 1
        snapshot.computationAttempts mustBe 1L
        snapshot.cacheReuses mustBe 1L
        snapshot.workloads(RunDiagnostics.Stage.Search).foldCompleted mustBe 1L
      }
    }

    "share fold scores and return the canonical candidate when only a fixed tracker differs" in {
      val strategy = TestStrategy.s5_optimized_v2
      val alias    = strategy.indicator match
        case Indicator.Composite(children, combinator) =>
          Indicator.Composite(
            children.map {
              case Indicator.ValueTracking(ValueRole.Momentum, source, _) =>
                Indicator.ValueTracking(ValueRole.Momentum, source, ValueTransformation.RSX(40))
              case child => child
            },
            combinator
          )
        case _ => fail("Expected s5 to be a composite")

      val result = for
        space <- IO.fromEither(IndicatorSearchSpace.forStrategy(strategy))
        calls <- Ref.of[IO, List[Indicator]](Nil)
        backtests = scores.map(score => (candidate: Indicator) => calls.update(_ :+ candidate).as(stats(score)))
        diagnostics <- RunDiagnostics.make[IO]
        evaluator   <- FoldRotatingEvaluator.cached[IO](backtests, scoring(), space.canonicalise, Some(diagnostics.evaluatorObserver))
        first       <- evaluator.evaluateIndividual(alias, EvaluationPhase.Search(0))
        second      <- evaluator.evaluateIndividual(strategy.indicator, EvaluationPhase.Rescore)
        recorded    <- calls.get
        snapshot    <- diagnostics.snapshot
      yield (first, second, recorded, snapshot)

      result.asserting { case (first, second, recorded, snapshot) =>
        alias must not be strategy.indicator
        first._1 mustBe strategy.indicator
        second._1 mustBe strategy.indicator
        recorded mustBe List.fill(scores.size)(strategy.indicator)
        snapshot.firstSeen mustBe Map(strategy.indicator -> 0)
        snapshot.distinctSearchCandidates mustBe 1
        snapshot.uniqueComputedCandidates mustBe 1
        snapshot.cacheReuses mustBe 1L
      }
    }

    "return a failed effect for an incompatible schema without backtesting or scoring it" in {
      val scoreCalls = new AtomicInteger(0)
      val result     = for
        space <- IO.fromEither(IndicatorSearchSpace.forStrategy(TestStrategy.s10))
        calls <- Ref.of[IO, List[Indicator]](Nil)
        backtests = scores.map(score => (candidate: Indicator) => calls.update(_ :+ candidate).as(stats(score)))
        diagnostics <- RunDiagnostics.make[IO]
        evaluator   <- FoldRotatingEvaluator
          .cached[IO](backtests, scoring { _ => scoreCalls.incrementAndGet(); () }, space.canonicalise, Some(diagnostics.evaluatorObserver))
        // Build the effects before attempting them: a synchronous exception must fail this test, rather than count as the expected Left.
        effects <- IO {
          List(
            evaluator.evaluateIndividual(indicator, EvaluationPhase.Search(0)),
            evaluator.evaluateIndividual(indicator, EvaluationPhase.Rescore)
          )
        }
        failures <- effects.traverse(_.attempt)
        recorded <- calls.get
        snapshot <- diagnostics.snapshot
      yield (failures, recorded, snapshot)

      result.asserting { case (failures, recorded, snapshot) =>
        failures.foreach {
          case Left(error: IllegalArgumentException) => error.getMessage mustBe "Indicator does not match this round's search-space schema"
          case other                                 => fail(s"Expected an effect containing a schema failure, got $other")
        }
        recorded mustBe Nil
        scoreCalls.get() mustBe 0
        snapshot mustBe RunDiagnostics.Snapshot()
      }
    }

    "reduce each fold to its score before starting the next backtest" in {
      val events    = new ConcurrentLinkedQueue[String]()
      val backtests = scores.zipWithIndex.map { case (score, fold) =>
        (_: Indicator) =>
          IO {
            events.add(s"backtest $fold")
            List(OrderStats(total = fold, totalProfit = BigDecimal(score)))
          }
      }
      val foldScoring = scoring { foldStats => events.add(s"score ${foldStats.head.total}"); () }
      val result      = for
        evaluator <- FoldRotatingEvaluator.cached[IO](backtests, foldScoring)
        _         <- evaluator.evaluateIndividual(indicator, EvaluationPhase.Search(0))
        _         <- evaluator.evaluateIndividual(indicator, EvaluationPhase.Rescore)
      yield events.iterator().asScala.toList

      result.asserting { recorded =>
        recorded mustBe List("backtest 0", "score 0", "backtest 1", "score 1", "backtest 2", "score 2")
      }
    }

    "retry a failed candidate and cache its successful fold scores" in {
      val failure = new RuntimeException("temporary backtest failure")
      val result  = for
        calls <- Ref.of[IO, Int](0)
        backtest = (_: Indicator) =>
          calls.getAndUpdate(_ + 1).flatMap {
            case 0 => IO.raiseError[List[OrderStats]](failure)
            case _ => IO.pure(stats(0.5))
          }
        diagnostics <- RunDiagnostics.make[IO]
        evaluator   <- FoldRotatingEvaluator.cached[IO](List(backtest), scoring(), observer = Some(diagnostics.evaluatorObserver))
        failed      <- evaluator.evaluateIndividual(indicator, EvaluationPhase.Search(0)).attempt
        retried     <- evaluator.evaluateIndividual(indicator, EvaluationPhase.Search(1))
        cached      <- evaluator.evaluateIndividual(indicator, EvaluationPhase.Rescore)
        count       <- calls.get
        snapshot    <- diagnostics.snapshot
      yield (failed, retried, cached, count, snapshot)

      result.asserting { case (failed, retried, cached, count, snapshot) =>
        failed mustBe Left(failure)
        retried mustBe cached
        cached._2.value mustBe 0.5
        count mustBe 2
        snapshot.searchRequests mustBe 2L
        snapshot.rescoreRequests mustBe 1L
        snapshot.successfulSearchRequests mustBe 1L
        snapshot.computationAttempts mustBe 2L
        snapshot.completedComputations mustBe 1L
        snapshot.uniqueComputedCandidates mustBe 1
        snapshot.cacheReuses mustBe 1L
        snapshot.firstSeen mustBe Map(indicator -> 1)
        snapshot.workloads(RunDiagnostics.Stage.Search).foldAttempts mustBe 2L
        snapshot.workloads(RunDiagnostics.Stage.Search).foldCompleted mustBe 1L
      }
    }

    "remember the best full-fold candidate even when rotating selection favours another and later loses it" in {
      val steady    = List(0.5, 0.5, 0.5)
      val brittle   = List(0.0, 1.0, 1.0)
      val backtests = steady.indices.toList.map { fold => (candidate: Indicator) =>
        IO.pure(stats(if (candidate == indicator) steady(fold) else brittle(fold)))
      }
      val result = for
        diagnostics    <- RunDiagnostics.make[IO]
        evaluator      <- FoldRotatingEvaluator.cached[IO](backtests, scoring(), observer = Some(diagnostics.evaluatorObserver))
        first          <- evaluator.evaluateIndividual(indicator, EvaluationPhase.Search(0))
        rotatingLeader <- evaluator.evaluateIndividual(other, EvaluationPhase.Search(0))
        _              <- evaluator.evaluateIndividual(other, EvaluationPhase.Search(2))
        _              <- evaluator.evaluateIndividual(other, EvaluationPhase.Rescore)
        snapshot       <- diagnostics.snapshot
      yield (first, rotatingLeader, snapshot)

      result.asserting { case (first, rotatingLeader, snapshot) =>
        rotatingLeader._2.value must be > first._2.value
        snapshot.bestSeen mustBe Some(RunDiagnostics.Discovery(indicator, IndicatorObjective.FoldAggregation.combine(steady), 0))
        snapshot.firstSeen mustBe Map(indicator -> 0, other -> 0)
        snapshot.computationAttempts mustBe 2L
      }
    }
  }
}
