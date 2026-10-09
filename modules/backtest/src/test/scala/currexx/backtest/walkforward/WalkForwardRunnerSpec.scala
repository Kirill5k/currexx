package currexx.backtest.walkforward

import cats.effect.{IO, Ref}
import cats.syntax.apply.*
import currexx.algorithms.{Fitness, Parameters}
import currexx.backtest.{MarketDataProvider, OptimisationRound, TestStrategy}
import currexx.backtest.MarketDataProvider.Dataset
import currexx.backtest.optimizer.{OptimisationResult, ScoringFunction, UpgradeDecision}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

class WalkForwardRunnerSpec extends IOWordSpec:
  private val base = TestStrategy(
    Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(10)),
    TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil)
  )
  private val candidate = base.copy(indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(20)))
  private val round     = OptimisationRound("test", base, Parameters.GA(2, 0, 0.0, 0.0, 0.0, shuffle = false), ScoringFunction.Consistent())
  private val plan      = WalkForwardPlan.validate(WalkForwardPlan.default.toOption.get.windows.take(2)).toOption.get
  private val experiment   = WalkForwardExperiment("test-experiment", round, 42L, MarketDataProvider.majors1hHistory.take(1), plan)
  private val finalists    = Vector((candidate.indicator, Fitness(2.0), Fitness(1.0)), (base.indicator, Fitness(1.0), Fitness(0.9)))
  private val optimisation =
    OptimisationResult(finalists, UpgradeFixtures.approved(base.indicator, candidate.indicator), UpgradeFixtures.evidence(base.indicator))
  private def metrics(net: Int): ForwardMetrics = ForwardMetrics(BigDecimal(net), 0, 0, 0, None, 0, 10000)

  "FrozenSelection" should {
    "convert the completed upgrade decision without applying another fitness gate" in {
      val zeroScore = optimisation.copy(finalists = Vector((candidate.indicator, Fitness(2.0), Fitness(0.0))))
      val frozen    = FrozenSelection.fromResult(base, zeroScore)
      frozen.strategy mustBe candidate
      frozen.strategy.rules mustBe base.rules
      frozen.outcome mustBe SelectionOutcome.UpgradeApproved
      frozen.selectionFitness mustBe Some(0.0)
      frozen.decision mustBe optimisation.decision
      frozen.baseEvidence mustBe optimisation.base
    }

    "retain the base when the policy rejects a candidate with positive fitness" in {
      val rejected = optimisation.copy(decision = UpgradeDecision.RetainBase(Nil))
      val frozen   = FrozenSelection.fromResult(base, rejected)
      frozen.strategy mustBe base
      frozen.outcome mustBe SelectionOutcome.BaseRetained
      frozen.selectionFitness mustBe Some(0.9)
      FrozenSelection.fromResult(base, rejected.copy(finalists = Vector.empty)).strategy mustBe base
    }
  }

  "WalkForwardRunner" should {
    "reject missing history in a later window before any search, reporting the affected window" in {
      val extendedPlan = WalkForwardPlan.validate(WalkForwardPlan.default.toOption.get.windows.take(3)).toOption.get
      val shortHistory =
        experiment.history.map(dataset => dataset.copy(filePaths = cats.data.NonEmptyList.fromListUnsafe(dataset.filePaths.toList.init)))
      val search = new WindowSearch[IO]:
        def search(request: OptimisationRound, seed: Long): IO[OptimisationResult] =
          IO.raiseError(new AssertionError("search called"))
      val forward = new ForwardEvaluator[IO]:
        def evaluate(selected: TestStrategy, original: TestStrategy, data: List[Dataset]): IO[ForwardResult] =
          IO.raiseError(new AssertionError("test called"))
      (for
        failures <- Ref.of[IO, List[Int]](Nil)
        events = WalkForwardEvents[IO](
          (_, _, _) => IO.raiseError(new AssertionError("selection recorded")),
          _ => IO.unit,
          (window, _) => failures.update(_ :+ window.index),
          _ => IO.raiseError(new AssertionError("invalid preflight persisted"))
        )
        result <- new WalkForwardRunner(search, forward, events).run(experiment.copy(history = shortHistory, plan = extendedPlan)).attempt
        failed <- failures.get
      yield {
        failed mustBe List(3)
        val error = result.swap.toOption.get
        error mustBe a[WalkForwardPreflight.Failure]
        error.getMessage must include("Missing requested months")
        error.getMessage must include("forward test")
      }).asserting(identity)
    }

    "stop before searching if the preflight report cannot be persisted" in {
      val search = new WindowSearch[IO]:
        def search(request: OptimisationRound, seed: Long): IO[OptimisationResult] =
          IO.raiseError(new AssertionError("search called"))
      val forward = new ForwardEvaluator[IO]:
        def evaluate(selected: TestStrategy, original: TestStrategy, data: List[Dataset]): IO[ForwardResult] =
          IO.raiseError(new AssertionError("test called"))
      val failure = new java.io.IOException("disk full")
      val events  = WalkForwardEvents[IO]((_, _, _) => IO.unit, _ => IO.unit, (_, _) => IO.unit, _ => IO.raiseError(failure))
      new WalkForwardRunner(search, forward, events).run(experiment).attempt.asserting { result =>
        result.swap.toOption.get.getCause mustBe failure
        result.swap.toOption.get.getMessage must include("persist preflight")
      }
    }

    "scope search to earlier data and persist each choice before testing, without feeding test results back" in {
      def execute(forwardNet: Int) = for
        log      <- Ref.of[IO, List[String]](Nil)
        requests <- Ref.of[IO, List[(OptimisationRound, Long)]](Nil)
        search = new WindowSearch[IO]:
          def search(request: OptimisationRound, seed: Long): IO[OptimisationResult] =
            requests.update(_ :+ (request -> seed)) >> log.update(_ :+ "search").as(optimisation)
        forward = new ForwardEvaluator[IO]:
          def evaluate(selected: TestStrategy, original: TestStrategy, data: List[Dataset]): IO[ForwardResult] =
            IO {
              selected mustBe candidate
              original mustBe base
              plan.windows.map(_.test) must contain(data.head.range.get)
            } >> log.update(_ :+ "test").as(ForwardResult(metrics(forwardNet), metrics(1), Nil))
        events = WalkForwardEvents[IO](
          (_, _, _) => log.update(_ :+ "frozen"),
          _ => log.update(_ :+ "completed"),
          (_, _) => log.update(_ :+ "failed"),
          _ => log.update(_ :+ "prepared")
        )
        results    <- new WalkForwardRunner(search, forward, events).run(experiment)
        seen       <- requests.get
        operations <- log.get
      yield (results, seen, operations)

      (execute(10), execute(-100)).tupled.asserting { case ((first, requests, operations), (second, otherRequests, _)) =>
        operations mustBe List("prepared", "search", "frozen", "test", "completed", "search", "frozen", "test", "completed")
        requests mustBe otherRequests
        first.map(_.selection) mustBe second.map(_.selection)
        first.map(_.seed) mustBe second.map(_.seed)
        requests.zip(plan.windows).foreach { case ((request, seed), window) =>
          request.corpus.searchFolds.map(_.head.range.get) mustBe window.trainingFolds
          request.corpus.validationFold.head.range mustBe Some(window.selection)
          request.strategy mustBe base
          seed mustBe window.seed(42L)
        }
        succeed
      }
    }

    "stop before the test if freezing fails, retaining the failure's window and cause" in {
      val failure = new RuntimeException("disk full")
      val search  = new WindowSearch[IO]:
        def search(request: OptimisationRound, seed: Long): IO[OptimisationResult] = IO.pure(optimisation)
      (for
        called   <- Ref.of[IO, Boolean](false)
        failures <- Ref.of[IO, List[Int]](Nil)
        forward = new ForwardEvaluator[IO]:
          def evaluate(selected: TestStrategy, original: TestStrategy, data: List[Dataset]): IO[ForwardResult] =
            called.set(true).as(ForwardResult(metrics(1), metrics(1), Nil))
        events = WalkForwardEvents[IO](
          (_, _, _) => IO.raiseError(failure),
          _ => IO.unit,
          (window, _) => failures.update(_ :+ window.index),
          _ => IO.unit
        )
        result <- new WalkForwardRunner(search, forward, events).run(experiment).attempt
        tested <- called.get
        failed <- failures.get
      yield {
        tested mustBe false
        failed mustBe List(1)
        result.swap.toOption.get.getCause mustBe failure
        result.swap.toOption.get.getMessage must include("persist selection")
      }).asserting(identity)
    }

    "persist a rejection before evaluating the retained base" in {
      val rejected = optimisation.copy(decision = UpgradeDecision.RetainBase(Nil))
      val search   = new WindowSearch[IO]:
        def search(request: OptimisationRound, seed: Long): IO[OptimisationResult] = IO.pure(rejected)
      (for
        persisted <- Ref.of[IO, Boolean](false)
        forward = new ForwardEvaluator[IO]:
          def evaluate(selected: TestStrategy, original: TestStrategy, data: List[Dataset]): IO[ForwardResult] =
            persisted.get.flatMap { saved =>
              IO {
                saved mustBe true
                selected mustBe original
                original mustBe base
                ForwardResult(metrics(1), metrics(1), Nil)
              }
            }
        events = WalkForwardEvents[IO](
          (_, _, frozen) => IO(frozen.decision mustBe rejected.decision) >> persisted.set(true),
          _ => IO.unit,
          (_, _) => IO.unit,
          _ => IO.unit
        )
        results <- new WalkForwardRunner(search, forward, events).run(experiment)
      yield results.foreach { result =>
        result.selection.outcome mustBe SelectionOutcome.BaseRetained
        result.forward.netDifference mustBe BigDecimal(0)
      }).asserting(_ => succeed)
    }

    "treat a search error as a failed experiment rather than base retention" in {
      val search = new WindowSearch[IO]:
        def search(request: OptimisationRound, seed: Long): IO[OptimisationResult] =
          IO.raiseError(new RuntimeException("search failed"))
      val forward = new ForwardEvaluator[IO]:
        def evaluate(selected: TestStrategy, original: TestStrategy, data: List[Dataset]): IO[ForwardResult] =
          IO.raiseError(new AssertionError("test called"))
      val events = WalkForwardEvents[IO](
        (_, _, _) => IO.raiseError(new AssertionError("selection recorded")),
        _ => IO.unit,
        (_, _) => IO.unit,
        _ => IO.unit
      )
      new WalkForwardRunner(search, forward, events).run(experiment).attempt.asserting(_.isLeft mustBe true)
    }
  }
