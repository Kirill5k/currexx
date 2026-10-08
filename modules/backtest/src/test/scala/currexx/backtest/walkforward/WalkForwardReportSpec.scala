package currexx.backtest.walkforward

import cats.effect.{IO, Resource}
import currexx.algorithms.Parameters
import currexx.backtest.{MarketDataProvider, OptimisationRound, TestStrategy}
import currexx.backtest.MarketDataProvider.Dataset
import currexx.backtest.optimizer.ScoringFunction
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
  private val selection = FrozenSelection(strategy, SelectionOutcome.BaseSelected, Some(0.5), Some(0.2))
  private val base      = ForwardMetrics(100, 4, 10, 1, Some(BigDecimal(2)), 3, 10000)
  private val coverage  = PeriodCoverage("EUR/USD", Instant.EPOCH, Instant.EPOCH.plusSeconds(3600), Instant.EPOCH.plusSeconds(7200), 0)
  private def result(index: Int, difference: Int, outcome: SelectionOutcome = SelectionOutcome.CandidateSelected): WindowResult =
    WindowResult(
      experiment.plan.windows(index - 1),
      experiment.plan.windows(index - 1).seed(experiment.masterSeed),
      selection.copy(outcome = outcome),
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
      manifest.get[String]("evidence") mustBe Right("retrospective development evidence")
      manifest.get[String]("seedDerivation") mustBe Right(WalkForwardWindow.seedVersion)
      manifest.downField("baseStrategy").focus mustBe Some(strategy.asJson)
      manifest.downField("search").downField("parameters").get[Int]("populationSize") mustBe Right(2)
      manifest.downField("search").get[String]("scoring").toOption.get must include("Consistent")
      manifest.downField("simulation").get[BigDecimal]("initialBalancePerPair") mustBe Right(BigDecimal(10000))
      manifest.downField("simulation").get[BigDecimal]("spreadPips") mustBe Right(BigDecimal("0.8"))
      manifest.downField("schedule").focus.flatMap(_.asArray).get must have size 5
      manifest.downField("sourceFiles").downArray.get[String]("sha256") mustBe Right("a" * 64)
      WalkForwardReportRenderer.frozen(window, 42L, selection).hcursor.downField("selection").downField("strategy").focus mustBe
        Some(strategy.asJson)
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

    "summarise paired differences without pooling drawdowns across independent windows" in {
      val results =
        List(result(1, 20), result(2, -10), result(3, 0, SelectionOutcome.BaseSelected), result(4, 0, SelectionOutcome.NoCandidatePassed))
      val totals = WalkForwardSummary.from(results)
      totals.completedWindows mustBe 4
      totals.totalNetDifference mustBe BigDecimal(10)
      totals.medianNetDifference mustBe Some(BigDecimal(0))
      totals.worstNetDifference mustBe Some(BigDecimal(-10))
      totals.positiveWindows mustBe 1
      totals.tiedWindows mustBe 2
      totals.negativeWindows mustBe 1
      totals.candidateSelectedWindows mustBe 2
      totals.baseSelectedWindows mustBe 1
      totals.noCandidatePassedWindows mustBe 1
      totals.baseRetainedWindows mustBe 2
      val summary = WalkForwardReportRenderer.summary(totals).hcursor
      summary.get[BigDecimal]("totalNetDifference") mustBe Right(BigDecimal(10))
      summary.get[BigDecimal]("medianNetDifference") mustBe Right(BigDecimal(0))
      summary.get[BigDecimal]("worstNetDifference") mustBe Right(BigDecimal(-10))
      summary.get[Int]("positiveWindows") mustBe Right(1)
      summary.get[Int]("tiedWindows") mustBe Right(2)
      summary.get[Int]("negativeWindows") mustBe Right(1)
      summary.get[Int]("baseRetainedWindows") mustBe Right(2)
      summary.get[Int]("baseSelectedWindows") mustBe Right(1)
      summary.get[Int]("noCandidatePassedWindows") mustBe Right(1)
      summary.downField("maxDrawdownPercent").focus mustBe None
      val markdown = WalkForwardReportRenderer.markdown(experiment, results, totals, None)
      markdown must include("not pooled into a continuous account")
      markdown must include("initial warm-up loss 0 bars")
      markdown must include("Base selected by ranking")
      markdown must include("Base retained: no candidate passed")
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
      val zero = WalkForwardSummary.from(List(result(1, 0, SelectionOutcome.NoCandidatePassed)))
      zero.medianNetDifference mustBe Some(BigDecimal(0))
      zero.worstNetDifference mustBe Some(BigDecimal(0))
      zero.tiedWindows mustBe 1
      zero.positiveWindows mustBe 0
      zero.negativeWindows mustBe 0
      zero.baseRetainedWindows mustBe 1
    }

    "leave unmeasured summary statistics empty until a window completes" in {
      val totals = WalkForwardSummary.from(Nil)
      totals mustBe WalkForwardSummary(0, BigDecimal(0), None, None, 0, 0, 0, 0, 0, 0)
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
            _             <- store.events.completed(result(1, 0, SelectionOutcome.BaseSelected))
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
            _         <- store.events.completed(result(1, 0, SelectionOutcome.BaseSelected))
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
