package currexx.backtest.optimizer

import cats.effect.IO
import cats.syntax.all.*
import currexx.algorithms.Parameters
import currexx.backtest.MarketDataProvider.{Corpus, Dataset, DateRange}
import currexx.backtest.{OptimisationRound, OrderStats, TestStrategy}
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
    extraSeeds = List(seedAlias),
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
        content must include(s"# Species-Conserving Genetic Algorithm (SCGA) Run: $label")
        content must include(s"**Parameters:** $params")
        content must include("## Final Results")
        content must include("**Top 2 members:**")
        content must include("2 finalist(s) validated")
        finalists.foreach { case (individual, _, _) => content must include(individual.toString) }
        content must include("SCGA validation replay completed.")
        content.indexOf("## Validation replay") must be > content.indexOf("## Final Results")
        (content must not).include("### Generation")
      }
    }

  }
}
