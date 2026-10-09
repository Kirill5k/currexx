package currexx.backtest.walkforward

import cats.effect.{IO, Resource}
import currexx.algorithms.Parameters
import currexx.backtest.{MarketDataProvider, OptimisationRound, TestStrategy}
import currexx.backtest.MarketDataProvider.Dataset
import currexx.backtest.optimizer.{ScoringFunction, SearchObjectiveConfig, UpgradeDecision, UpgradeFailure, UpgradeRejection}
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation}
import fs2.io.file.Path
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import kirill5k.common.cats.test.IOWordSpec

import java.io.IOException
import java.nio.file.Files
import java.time.Instant
import scala.jdk.CollectionConverters.*

class WalkForwardReportSpec extends IOWordSpec {
  private val strategy = TestStrategy(
    Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(10)),
    TradeStrategy(Nil, Nil)
  )
  private val round      = OptimisationRound("report", strategy, Parameters.GA(2, 0, 0.7, 0.1, 0.02, false), ScoringFunction.Consistent())
  private val source     = "eur-usd-1h-walk-forward-full.csv"
  private val experiment = WalkForwardExperiment("report-test", round, 42L, List(Dataset(source)), WalkForwardPlan.default.toOption.get)
  private val window     = experiment.plan.windows.head
  private val readiness  = List(
    WindowReadiness(window, List(PeriodReadiness("training fold 1", window.trainingFolds.head, "EUR/USD", 120, 0, 99)))
  )
  private val selection =
    FrozenSelection(strategy, Some(0.5), Some(0.2), UpgradeDecision.RetainBase(Nil), UpgradeFixtures.evidence(strategy.indicator))
  private val base     = ForwardMetrics(100, 4, 10, 1, Some(BigDecimal(2)), 3, 10000)
  private val coverage = PeriodCoverage("EUR/USD", Instant.EPOCH, Instant.EPOCH.plusSeconds(3600), Instant.EPOCH.plusSeconds(7200), 0)
  private def result(index: Int, difference: Int, outcome: SelectionOutcome = SelectionOutcome.UpgradeApproved): WindowResult =
    WindowResult(
      experiment.plan.windows(index - 1),
      experiment.plan.windows(index - 1).seed(experiment.masterSeed),
      selection.copy(decision = outcome match
        case SelectionOutcome.UpgradeApproved => UpgradeFixtures.approved(strategy.indicator, strategy.indicator)
        case SelectionOutcome.BaseRetained    => UpgradeDecision.RetainBase(Nil)),
      ForwardResult(base.copy(netProfit = base.netProfit + difference), base, List(coverage))
    )

  private def temporaryDirectory: Resource[IO, Path] = Resource
    .make(IO.blocking(Files.createTempDirectory("walkforward-report-test"))) { path =>
      IO.blocking {
        val entries = Files.walk(path)
        try entries.iterator().asScala.toList.reverse.foreach(Files.delete)
        finally entries.close()
      }
    }
    .map(Path.fromNioPath)

  private def read(path: Path): IO[String]   = IO.blocking(Files.readString(path.toNioPath))
  private def readJson(path: Path): IO[Json] = read(path).flatMap(value => IO.fromEither(parse(value)))

  "WalkForwardReportRenderer" should {
    "record full strategies, explicit configuration and chronology in the manifest" in {
      val manifest = WalkForwardReportRenderer.manifest(experiment, List(source -> "a" * 64)).hcursor
      manifest.get[Int]("formatVersion") mustBe Right(2)
      manifest.downField("search").downField("objective").get[String]("mode") mustBe Right("Current")
      manifest.downField("search").downField("upgradePolicy").get[Int]("minMonths") mustBe Right(3)
      manifest.get[String]("evidence") mustBe Right("retrospective development evidence")
      manifest.get[String]("seedDerivation") mustBe Right(WalkForwardWindow.seedVersion)
      manifest.downField("baseStrategy").focus mustBe Some(strategy.asJson)
      manifest.downField("search").downField("parameters").get[Int]("populationSize") mustBe Right(2)
      manifest.downField("search").get[String]("scoring").toOption.get must include("Consistent")
      manifest.downField("simulation").get[BigDecimal]("initialBalancePerPair") mustBe Right(BigDecimal(10000))
      manifest.downField("simulation").get[BigDecimal]("spreadPips") mustBe Right(BigDecimal("0.8"))
      manifest.downField("schedule").focus.flatMap(_.asArray).get must have size 5
      manifest.downField("sourceFiles").downArray.get[String]("sha256") mustBe Right("a" * 64)
      val frozen = WalkForwardReportRenderer.frozen(window, 42L, selection).hcursor
      frozen.get[Int]("formatVersion") mustBe Right(2)
      frozen.downField("selection").downField("strategy").focus mustBe Some(strategy.asJson)
      frozen.downField("selection").downField("upgradeDecision").get[String]("outcome") mustBe Right("BASE RETAINED")
      frozen.downField("selection").downField("baseEvidence").get[Int]("monthsCovered") mustBe Right(4)
    }

    "include every SCGA setting and effective species cap" in {
      val params = Parameters.SCGA(4, 1, 0.7, 0.1, true, 2, 0.2, 8, 0.3)
      val cursor = WalkForwardReportRenderer
        .manifest(experiment.copy(round = round.copy(parameters = params)), Nil)
        .hcursor
        .downField("search")
        .downField("parameters")
      cursor.get[String]("algorithm") mustBe Right("SCGA")
      cursor.get[Double]("speciesRadius") mustBe Right(0.2)
      cursor.get[Int]("maxSpecies") mustBe Right(8)
      cursor.get[Int]("effectiveMaxSpecies") mustBe Right(2)
      cursor.get[Double]("interspeciesMatingProbability") mustBe Right(0.3)
      cursor.get[Int]("initialOversampling") mustBe Right(2)
    }

    "persist rejected comparisons and their exact policy reasons before forward evidence exists" in {
      val approval  = UpgradeFixtures.approved(strategy.indicator, strategy.indicator)
      val rejection = UpgradeRejection(
        strategy.indicator,
        approval.comparison,
        List(UpgradeFailure("net_improvement", "19.999", ">= 20"))
      )
      val rejected = selection.copy(decision = UpgradeDecision.RetainBase(List(rejection)))
      val frozen   = WalkForwardReportRenderer.frozen(window, 42L, rejected).hcursor
      val decision = frozen.downField("selection").downField("upgradeDecision")
      decision.downField("rejections").downArray.downField("failures").downArray.get[String]("code") mustBe Right("net_improvement")
      decision.downField("rejections").downArray.downField("failures").downArray.get[String]("actual") mustBe Right("19.999")
      decision.downField("rejections").downArray.downField("comparison").get[BigDecimal]("stressedNetImprovement") mustBe Right(
        BigDecimal(100)
      )
      frozen.downField("forward").focus mustBe None
    }

    "compare modes and seeds with honest source labels and separate forward summaries" in {
      val current  = WalkForwardComparison.Run(experiment, WalkForwardSummary.from(List(result(1, 20))))
      val relative = WalkForwardComparison.Run(
        experiment
          .copy(id = "relative-test", round = round.copy(searchObjective = SearchObjectiveConfig.BaselineRelative()), masterSeed = 43L),
        WalkForwardSummary.from(List(result(1, -10), result(2, 0, SelectionOutcome.BaseRetained)))
      )
      val report = WalkForwardComparison.markdown(List(current, relative))
      report must include("retrospective development evidence")
      report must include("BaselineRelative")
      report must include("| 43 | -10 | -5 | -10 | 0/2 | 1 |")
      report must include("| 42 | 20 | 20 | 20 | 1/1 | 1 |")
      val json = WalkForwardComparison.json(List(current, relative)).hcursor
      json.get[Int]("formatVersion") mustBe Right(2)
      json.downField("runs").downArray.right.downField("objective").get[Double]("weight") mustBe Right(0.5)
    }

    "summarise paired differences without pooling drawdowns across independent windows" in {
      val results =
        List(result(1, 20), result(2, -10), result(3, 0, SelectionOutcome.BaseRetained), result(4, 0, SelectionOutcome.BaseRetained))
      val totals = WalkForwardSummary.from(results)
      totals.completedWindows mustBe 4
      totals.totalNetDifference mustBe BigDecimal(10)
      totals.medianNetDifference mustBe Some(BigDecimal(0))
      totals.worstNetDifference mustBe Some(BigDecimal(-10))
      totals.positiveWindows mustBe 1
      totals.tiedWindows mustBe 2
      totals.negativeWindows mustBe 1
      totals.upgradeApprovedWindows mustBe 2
      totals.baseRetainedWindows mustBe 2
      val summary = WalkForwardReportRenderer.summary(totals).hcursor
      summary.get[BigDecimal]("totalNetDifference") mustBe Right(BigDecimal(10))
      summary.get[BigDecimal]("medianNetDifference") mustBe Right(BigDecimal(0))
      summary.get[BigDecimal]("worstNetDifference") mustBe Right(BigDecimal(-10))
      summary.get[Int]("positiveWindows") mustBe Right(1)
      summary.get[Int]("tiedWindows") mustBe Right(2)
      summary.get[Int]("negativeWindows") mustBe Right(1)
      summary.get[Int]("baseRetainedWindows") mustBe Right(2)
      summary.get[Int]("upgradeApprovedWindows") mustBe Right(2)
      summary.downField("maxDrawdownPercent").focus mustBe None
      val markdown = WalkForwardReportRenderer.markdown(experiment, results, totals, None)
      markdown must include("not pooled into a continuous account")
      markdown must include("initial warm-up loss 0 bars")
      markdown must include("UPGRADE APPROVED")
      markdown must include("BASE RETAINED")
    }

    "keep profit factors as separate measurements without subtracting ratios" in {
      val measured = result(1, 0).copy(forward = ForwardResult(base.copy(profitFactor = Some(BigDecimal("1.25"))), base, List(coverage)))
      val missing  = measured.copy(forward = measured.forward.copy(candidate = base.copy(profitFactor = None)))
      val measuredJson = WalkForwardReportRenderer.completed(measured).hcursor.downField("forward")
      measuredJson.downField("candidate").get[BigDecimal]("profitFactor") mustBe Right(BigDecimal("1.25"))
      measuredJson.downField("base").get[BigDecimal]("profitFactor") mustBe Right(BigDecimal(2))
      measuredJson.downField("difference").downField("profitFactor").focus mustBe None
      val missingJson = WalkForwardReportRenderer.completed(missing).hcursor.downField("forward")
      missingJson.downField("candidate").downField("profitFactor").focus mustBe Some(Json.Null)
      missingJson.downField("difference").downField("profitFactor").focus mustBe None
      WalkForwardReportRenderer.markdown(experiment, List(measured), WalkForwardSummary.from(List(measured)), None) must
        include("| Profit factor | 1.25 | 2 | — |")
      WalkForwardReportRenderer.markdown(experiment, List(missing), WalkForwardSummary.from(List(missing)), None) must
        include("| Profit factor | n/a | 2 | — |")
    }

    "use an arithmetic median for even window counts and preserve exact ties" in {
      val totals = WalkForwardSummary.from(List(result(1, -1), result(2, 12)))
      totals.medianNetDifference mustBe Some(BigDecimal("5.5"))
      totals.totalNetDifference mustBe BigDecimal(11)
      val zero = WalkForwardSummary.from(List(result(1, 0, SelectionOutcome.BaseRetained)))
      zero.medianNetDifference mustBe Some(BigDecimal(0))
      zero.worstNetDifference mustBe Some(BigDecimal(0))
      zero.tiedWindows mustBe 1
      zero.positiveWindows mustBe 0
      zero.negativeWindows mustBe 0
      zero.baseRetainedWindows mustBe 1
    }

    "leave unmeasured summary statistics empty until a window completes" in {
      val totals = WalkForwardSummary.from(Nil)
      totals mustBe WalkForwardSummary(0, BigDecimal(0), None, None, 0, 0, 0, 0, 0)
      totals.baseRetainedWindows mustBe 0
      val summary = WalkForwardReportRenderer.summary(totals).hcursor
      summary.get[BigDecimal]("totalNetDifference") mustBe Right(BigDecimal(0))
      summary.downField("medianNetDifference").focus mustBe Some(Json.Null)
      summary.downField("worstNetDifference").focus mustBe Some(Json.Null)
      WalkForwardReportRenderer.markdown(experiment, Nil, totals, None) must include("Total net difference: 0; median: n/a; worst: n/a.")
    }
  }

  "WalkForwardReportStore" should {
    "persist the manifest and frozen strategy before any result, then retain completed records on failure" in
      temporaryDirectory
        .use { root =>
          for
            store         <- WalkForwardReportStore.make[IO](experiment, root)
            _             <- store.events.prepared(readiness)
            preflight     <- readJson(store.directory / "preflight.json")
            beforeSearch  <- read(store.directory / "report.md")
            expectedHash  <- MarketDataProvider.fingerprint[IO](source)
            manifest      <- readJson(store.directory / "manifest.json")
            _             <- store.events.frozen(window, window.seed(42L), selection)
            frozen        <- readJson(store.directory / "window-1-frozen.json")
            hasResult     <- IO.blocking(Files.exists((store.directory / "window-1-result.json").toNioPath))
            _             <- store.events.completed(result(1, 0, SelectionOutcome.BaseRetained))
            beforeFailure <- readJson(store.directory / "window-1-result.json")
            _             <- store.events.failed(
              experiment.plan.windows(1),
              new IllegalStateException("Window 2 (persist result) failed: disk full", new IOException("disk full"))
            )
            retained <- readJson(store.directory / "window-1-result.json")
            failure  <- readJson(store.directory / "window-2-failure.json")
            summary  <- readJson(store.directory / "summary.json")
            report   <- read(store.directory / "report.md")
          yield
            preflight.hcursor.downArray.get[Int]("windowIndex") mustBe Right(1)
            preflight.hcursor.downArray.downField("periods").downArray.get[Int]("warmupBarsLost") mustBe Right(99)
            preflight.hcursor.downArray.downField("periods").downArray.get[Long]("executableBars") mustBe Right(20L)
            beforeSearch must include("| 1 | training fold 1 | EUR/USD | 2023-07..2023-10 | 99 | 20 |")
            manifest.hcursor.downField("sourceFiles").downArray.get[String]("sha256") mustBe Right(expectedHash)
            frozen.hcursor.downField("selection").downField("strategy").focus mustBe Some(strategy.asJson)
            hasResult mustBe false
            retained mustBe beforeFailure
            failure.hcursor.get[Int]("windowIndex") mustBe Right(2)
            failure.hcursor.get[String]("errorType") mustBe Right("java.io.IOException")
            failure.hcursor.get[String]("message") mustBe Right("Window 2 (persist result) failed: disk full")
            summary.hcursor.get[String]("status") mustBe Right("failed")
            summary.hcursor.downField("summary").get[Int]("completedWindows") mustBe Right(1)
            report must include("Failed window 2")
            report must include("| 1 | training fold 1 | EUR/USD | 2023-07..2023-10 | 99 | 20 |")
            report must include("java.io.IOException")
            report must include("Window 2 (persist result) failed: disk full")
        }
        .asserting(identity)

    "mark the last successful window as completed and preserve existing directories" in
      temporaryDirectory
        .use { root =>
          val single = experiment.copy(plan = WalkForwardPlan.validate(List(window)).toOption.get)
          for
            store     <- WalkForwardReportStore.make[IO](single, root)
            _         <- store.events.frozen(window, window.seed(42L), selection)
            _         <- store.events.completed(result(1, 0, SelectionOutcome.BaseRetained))
            summary   <- readJson(store.directory / "summary.json")
            collision <- WalkForwardReportStore.make[IO](single, root).attempt
            retained  <- readJson(store.directory / "summary.json")
          yield
            summary.hcursor.get[String]("status") mustBe Right("completed")
            collision.isLeft mustBe true
            retained mustBe summary
        }
        .asserting(identity)

    "propagate required writes that fail instead of reporting a successful selection" in
      temporaryDirectory
        .use { root =>
          for
            store  <- WalkForwardReportStore.make[IO](experiment, root)
            _      <- IO.blocking(Files.createDirectory((store.directory / "window-1-frozen.json").toNioPath))
            failed <- store.events.frozen(window, window.seed(42L), selection).attempt
          yield failed.isLeft mustBe true
        }
        .asserting(identity)
  }
}
