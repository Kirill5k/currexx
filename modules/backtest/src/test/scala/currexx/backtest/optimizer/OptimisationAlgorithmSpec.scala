package currexx.backtest.optimizer

import cats.effect.IO
import cats.syntax.all.*
import currexx.algorithms.Parameters
import currexx.backtest.MarketDataProvider.{Corpus, Dataset, DateRange}
import currexx.backtest.{NamedIndicator, OptimisationRound, OrderStats, TestStrategy}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import fs2.io.file.{Files, Path}
import kirill5k.common.cats.test.IOWordSpec

import java.time.{YearMonth, ZoneOffset}
import java.util.UUID
import scala.concurrent.duration.*
import scala.util.Random

class OptimisationAlgorithmSpec extends IOWordSpec {
  private val frozen = Indicator.VolatilityRegimeDetection(10, VT.SMA(20))
  private val raw    = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1))
  private val unread = Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(12))

  private def indicator(length: Int): Indicator =
    Indicator.compositeAnyOf(Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(length)), frozen, raw, unread)

  private val strategy = TestStrategy(
    indicator(10),
    TradeStrategy(
      List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)),
      List(Rule(TradeAction.ClosePosition, Rule.Condition.PositionOpenFor(4.hours)))
    )
  )

  private val seedAlias = Indicator.compositeAnyOf(
    Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(70)),
    Indicator.VolatilityRegimeDetection(25, VT.SMA(50)),
    Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(30)),
    Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(40))
  )

  private def dataset(month: Int): Dataset = Dataset(
    "aud-usd-1h-1year-2023-07-2024-06.csv",
    Some(DateRange(YearMonth.of(2023, month), YearMonth.of(2023, month + 1)))
  )

  private val corpus = Corpus(List(List(dataset(8)), List(dataset(9))), List(dataset(10)))

  // Calendar-based scores make corpus, scoring-function and final all-fold aggregation wiring observable without choosing market winners.
  private val scoring = new ScoringFunction {
    override def score(stats: List[OrderStats]): Double =
      stats.flatMap(_.dataWindow).map(_.from.atZone(ZoneOffset.UTC).getMonthValue.toDouble).sum
    override def violations(stats: List[OrderStats]): List[ScoringFunction.Violation] = Nil
  }

  private def round(name: String, params: Parameters.GA | Parameters.SCGA): OptimisationRound = OptimisationRound(
    name = name,
    strategy = strategy,
    parameters = params,
    scoringFunction = scoring,
    corpus = corpus,
    shortlistSize = 2,
    extraSeeds = List(NamedIndicator("alternative", seedAlias)),
    fixedIndicators = Set(frozen)
  )

  private def readReports(label: String): IO[List[(Path, String)]] = {
    val files = Files.forAsync[IO]
    files
      .list(Path("optimisation-results"))
      .filter(_.fileName.toString.endsWith(s"-$label.md"))
      .compile
      .toList
      .bracket(paths => paths.traverse(path => files.readUtf8(path).compile.string.map(path -> _)))(paths =>
        paths.traverse_(files.deleteIfExists)
      )
  }

  "The round-configured optimisation dispatcher" should {
    "reject invalid baseline budgets before loading the corpus or creating a report" in {
      given random: Random = Random(91)
      val params           = Parameters.GA(1, 0, 0.0, 0.0, 0.0, shuffle = false)
      val missingCorpus    = Corpus(List(List(Dataset("missing-baseline-budget-test.csv"))), Nil)
      val cases            = List(0, -1, 1).map(size => (s"baseline-budget-$size-${UUID.randomUUID()}", size))

      cases
        .traverse { case (label, size) =>
          for
            result <- OptimisationAlgorithm
              .indicator[IO](round(label, params).copy(shortlistSize = size, corpus = missingCorpus), 1)
              .attempt
            reports <- readReports(label)
          yield (result, reports)
        }
        .asserting { results =>
          results.foreach { case (result, reports) =>
            result match
              case Left(error: IllegalArgumentException) => error.getMessage.toLowerCase must include("shortlist")
              case other                                 => fail(s"Expected baseline budget failure, got $other")
            reports mustBe empty
          }
          random.nextLong() mustBe Random(91).nextLong()
        }
    }

    "reserve a seed omitted from initialisation without inventing a search discovery" in {
      given Random = Random(91)
      val params   = Parameters.GA(1, 0, 0.0, 0.0, 0.0, shuffle = false)
      val label    = s"unsearched-baseline-${UUID.randomUUID()}"

      val result = for
        algorithm <- OptimisationAlgorithm.indicator[IO](round(label, params), 1)
        finalists <- algorithm.optimise
        reports   <- readReports(label)
      yield (finalists, reports)

      result.asserting { case (finalists, reports) =>
        finalists.map(_._1) mustBe Vector(strategy.indicator, indicator(70))
        val content = reports.head._2
        content must include("Selection source: target retained.")
        content must include("#2: first seen=not observed; baselines=alternative (fixed inputs restored)")
        content must include("Search evaluation requests: 1; successful: 1; distinct successfully searched candidates: 1.")
        content must include("Final rescore requests: 2")
        content must include("Validation: candidate requests=2; folds=2/2 completed/attempted")
        reports must have size 1
      }
    }

    "start a fresh archive for each invocation of the same configured optimiser" in {
      // Each zero-generation run has the target and one distinct jitter. The earlier jitter wins lexical ties,
      // so retaining the previous archive would incorrectly select it again on the second invocation.
      class ControlledJitterRandom extends Random(91) {
        var jitter: Double                  = 1.0
        override def nextGaussian(): Double = jitter
      }
      val random   = new ControlledJitterRandom
      given Random = random
      val params   = Parameters.GA(2, 0, 0.0, 0.0, 0.0, shuffle = false)
      val label    = s"fresh-run-archive-${UUID.randomUUID()}"

      val result = for
        algorithm <- OptimisationAlgorithm.indicator[IO](round(label, params).copy(extraSeeds = Nil), 1)
        first     <- algorithm.optimise
        _         <- IO { random.jitter = 2.0 }
        second    <- algorithm.optimise
        _         <- readReports(label)
      yield (first, second)

      result.asserting { case (first, second) =>
        first.map(_._1).head mustBe strategy.indicator
        second.map(_._1).head mustBe strategy.indicator
        first must have size 2
        second must have size 2
        first.last._1.toString must be < second.last._1.toString
        second.map(_._1) must not contain first.last._1
      }
    }

    "wire GA seeds, fixed inputs, corpus, scorer, shortlist and final progress with reproducible validation" in {
      given Random = Random(91)
      val params   = Parameters.GA(2, 0, 0.0, 0.0, 0.0, shuffle = false)
      val label    = s"ga-factory-test-${UUID.randomUUID()}"

      val result = for
        algorithm   <- OptimisationAlgorithm.indicator[IO](round(label, params), evaluatorPoolSize = 1)
        finalists   <- algorithm.optimise
        replayed    <- finalists.map(_._1).traverse(algorithm.validate)
        aliasReplay <- algorithm.validate(seedAlias)
        _ <- algorithm.tracker.displayNote("Validation replay", List("Replayed both finalists through the configured validation corpus."))
        reports <- readReports(label)
      yield (algorithm.searchSpace, finalists, replayed, aliasReplay, reports)

      result.asserting { case (space, finalists, replayed, aliasReplay, reports) =>
        space.template mustBe strategy.indicator
        space.fixedIndicators mustBe Set(frozen, raw, unread)
        space.canonicalise(seedAlias) mustBe Right(indicator(70))
        finalists.map(_._1).toSet mustBe Set(strategy.indicator, indicator(70))
        finalists.foreach { case (_, training, validation) =>
          training.value mustBe IndicatorObjective.FoldAggregation.combine(List(8.0, 9.0))
          validation.value mustBe 10.0
        }
        replayed.map(scoring.score) mustBe finalists.map(_._3.value)
        scoring.score(aliasReplay) mustBe 10.0
        replayed.flatten.foreach { stats =>
          stats.total must be > 0
          stats.dataWindow.map(window => YearMonth.from(window.from.atZone(ZoneOffset.UTC))) mustBe Some(YearMonth.of(2023, 10))
          stats.dataWindow.map(window => YearMonth.from(window.to.atZone(ZoneOffset.UTC))) mustBe Some(YearMonth.of(2023, 10))
        }
        reports must have size 1
        val (path, content) = reports.head
        path.fileName.toString must startWith("ga-optimisation-")
        content must include(s"# Genetic Algorithm Run: $label")
        content must include(s"**Target:** ${strategy.indicator}")
        content must include(s"**Parameters:** $params")
        content must include("## Final Results")
        content must include("**Top 2 members:**")
        content must include("2 finalist(s) validated")
        content must include(s"## Champion selection: $label")
        content must include("alternative")
        content must include("first successful search")
        finalists.foreach { case (individual, _, _) => content must include(individual.toString) }
        content must include("Replayed both finalists through the configured validation corpus.")
        content.indexOf("## Validation replay") must be > content.indexOf("## Final Results")
        (content must not).include("### Generation")
      }
    }

    "wire SCGA species settings and preserve canonical species representatives through one generation and shortlisting" in {
      given Random = Random(117)
      val params   = Parameters.SCGA(
        populationSize = 4,
        maxGen = 1,
        crossoverProbability = 0.0,
        mutationProbability = 0.0,
        shuffle = false,
        speciesRadius = 0.01,
        maxSpecies = 2,
        interspeciesMatingProbability = 0.0
      )
      val label = s"scga-factory-test-${UUID.randomUUID()}"

      val result = for
        algorithm <- OptimisationAlgorithm.indicator[IO](round(label, params), evaluatorPoolSize = 1)
        finalists <- algorithm.optimise
        replayed  <- finalists.map(_._1).traverse(algorithm.validate)
        _         <- algorithm.tracker.displayNote("Validation replay", List("SCGA validation replay completed."))
        reports   <- readReports(label)
      yield (algorithm.searchSpace, finalists, replayed, reports)

      result.asserting { case (space, finalists, replayed, reports) =>
        space.fixedIndicators mustBe Set(frozen, raw, unread)
        finalists must have size 2
        IndicatorDistance.make(space).between(finalists.head._1, finalists.last._1).exists(_ > params.speciesRadius) mustBe true
        finalists.foreach { case (individual, training, validation) =>
          space.canonicalise(individual) mustBe Right(individual)
          training.value mustBe IndicatorObjective.FoldAggregation.combine(List(8.0, 9.0))
          validation.value mustBe 10.0
        }
        replayed.map(scoring.score) mustBe finalists.map(_._3.value)
        reports must have size 1
        val (path, content) = reports.head
        path.fileName.toString must startWith("scga-optimisation-")
        content must include(s"# Species-Conserving Genetic Algorithm Run: $label")
        content must include(s"**Parameters:** $params")
        content must include("## Final Results")
        content must include("**Top 2 members:**")
        content must include("2 finalist(s) validated")
        content must include(s"## Champion selection: $label")
        finalists.foreach { case (individual, _, _) => content must include(individual.toString) }
        content must include("SCGA validation replay completed.")
        content.indexOf("## Validation replay") must be > content.indexOf("## Final Results")
        (content must not).include("### Generation")
      }
    }

    "return completed finalists and continue queued rounds after a diagnostic replay failure" in {
      given Random           = Random(17)
      val params             = Parameters.GA(2, 0, 0.0, 0.0, 0.0, shuffle = false)
      val label              = s"diagnostics-failure-test-${UUID.randomUUID()}"
      val nextLabel          = s"diagnostics-next-round-test-${UUID.randomUUID()}"
      val failure            = new IllegalStateException("Deliberate diagnostics failure")
      val failingDiagnostics = new ScoringFunction {
        override def score(stats: List[OrderStats]): Double                               = scoring.score(stats)
        override def violations(stats: List[OrderStats]): List[ScoringFunction.Violation] = throw failure
      }

      val result = for
        finalists <- List(round(label, params).copy(scoringFunction = failingDiagnostics), round(nextLabel, params)).traverse { config =>
          OptimisationAlgorithm.indicator[IO](config, 1).flatMap(_.optimise)
        }
        reports     <- readReports(label)
        nextReports <- readReports(nextLabel)
      yield (finalists, reports, nextReports)

      result.asserting { case (finalists, reports, nextReports) =>
        finalists must have size 2
        finalists.head mustBe finalists.last
        finalists.head.map(_._1).toSet mustBe Set(strategy.indicator, indicator(70))
        finalists.head.foreach { case (_, training, validation) =>
          training.value mustBe IndicatorObjective.FoldAggregation.combine(List(8.0, 9.0))
          validation.value mustBe 10.0
        }
        reports must have size 1
        val content = reports.head._2
        content must include("## Final Results")
        content must include("2 finalist(s) validated")
        content must include(s"## Incomplete diagnostics: $label")
        content must include("Deliberate diagnostics failure")
        content must include("Remaining rounds can continue.")
        content.indexOf("## Incomplete diagnostics") must be > content.indexOf("## Final Results")
        nextReports must have size 1
        nextReports.head._2 must include(s"## Champion selection: $nextLabel")
      }
    }

    "propagate search failures without labelling them as incomplete diagnostics" in {
      given Random      = Random(17)
      val params        = Parameters.GA(2, 0, 0.0, 0.0, 0.0, shuffle = false)
      val label         = s"search-failure-test-${UUID.randomUUID()}"
      val failure       = new IllegalStateException("Deliberate search failure")
      val failingSearch = new ScoringFunction {
        override def score(stats: List[OrderStats]): Double                               = throw failure
        override def violations(stats: List[OrderStats]): List[ScoringFunction.Violation] = Nil
      }

      val result = for
        algorithm <- OptimisationAlgorithm.indicator[IO](round(label, params).copy(scoringFunction = failingSearch), 1)
        outcome   <- algorithm.optimise.attempt
        reports   <- readReports(label)
      yield (outcome, reports)

      result.asserting { case (outcome, reports) =>
        outcome mustBe Left(failure)
        reports must have size 1
        (reports.head._2 must not).include("## Final Results")
        (reports.head._2 must not).include("## Incomplete diagnostics")
      }
    }

    "propagate final validation failures instead of treating them as optional diagnostics" in {
      given Random          = Random(17)
      val params            = Parameters.GA(1, 0, 0.0, 0.0, 0.0, shuffle = false)
      val label             = s"validation-failure-test-${UUID.randomUUID()}"
      val failure           = new IllegalStateException("Deliberate validation failure")
      val failingValidation = new ScoringFunction {
        override def score(stats: List[OrderStats]): Double =
          if (scoring.score(stats) == 10.0) throw failure else scoring.score(stats)
        override def violations(stats: List[OrderStats]): List[ScoringFunction.Violation] = Nil
      }

      val result = for
        algorithm <- OptimisationAlgorithm.indicator[IO](round(label, params).copy(scoringFunction = failingValidation), 1)
        outcome   <- algorithm.optimise.attempt
        reports   <- readReports(label)
      yield (outcome, reports)

      result.asserting { case (outcome, reports) =>
        outcome mustBe Left(failure)
        reports must have size 1
        (reports.head._2 must not).include("## Final Results")
        (reports.head._2 must not).include("## Incomplete diagnostics")
      }
    }

  }
}
