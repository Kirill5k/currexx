package currexx.backtest.optimizer.reporting

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import currexx.algorithms.{Fitness, Parameters, ValidatedPopulation}
import currexx.algorithms.operators.{Elitism, Selector, Validator}
import currexx.algorithms.operators.species.SpeciesOperators
import currexx.algorithms.progress.{Progress, Tracker}
import currexx.backtest.{OrderStats, TestStrategy}
import currexx.backtest.optimizer.{
  FoldRotatingEvaluator,
  IndicatorDistance,
  IndicatorObjective,
  IndicatorSearchOperators,
  IndicatorSearchSpace,
  OptimisationAlgorithm,
  ScoringFunction
}
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class ReportingSearchInvarianceSpec extends IOWordSpec {
  private val foldCount    = 3
  private val target       = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(20))
  private val strategy     = TestStrategy(target, TradeStrategy(Nil, Nil))
  private val gaParameters = Parameters.GA(
    populationSize = 8,
    maxGen = 5,
    crossoverProbability = 0.8,
    mutationProbability = 0.65,
    elitismRatio = 0.25,
    shuffle = true,
    initialOversampling = 3
  )

  private val scoring = new ScoringFunction {
    override def score(stats: List[OrderStats]): Double                               = stats.map(_.totalProfit.toDouble).sum
    override def violations(stats: List[OrderStats]): List[ScoringFunction.Violation] = Nil
  }

  private def length(indicator: Indicator): Int = indicator match {
    case Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(n)) => n
    case other                                                        => throw new IllegalArgumentException(s"Unexpected candidate: $other")
  }

  private def foldScore(indicator: Indicator, fold: Int): Double =
    300.0 - math.abs(length(indicator) - (25 + fold * 30))

  private def stableScore(indicator: Indicator): Double =
    IndicatorObjective.FoldAggregation.combine(List.tabulate(foldCount)(foldScore(indicator, _)))

  final private case class Outcome(
      population: ValidatedPopulation[Indicator],
      nextRandom: Long,
      foldCalls: Map[(Indicator, Int), Int],
      validations: Vector[Indicator],
      snapshot: Option[RunDiagnostics.Snapshot],
      noteTitles: Vector[String]
  )

  private def run(params: Parameters.GA | Parameters.SCGA, instrumented: Boolean): IO[Outcome] = IO.defer {
    // Reusing this IO must recreate the generator, including the initial oversampling draws.
    given random: Random = Random(42)
    for {
      space       <- IO.fromEither(IndicatorSearchSpace.forStrategy(strategy))
      search      <- IndicatorSearchOperators.make[IO](space)
      calls       <- Ref.of[IO, Map[(Indicator, Int), Int]](Map.empty)
      validations <- Ref.of[IO, Vector[Indicator]](Vector.empty)
      notes       <- Ref.of[IO, Vector[String]](Vector.empty)
      diagnostics <- if (instrumented) RunDiagnostics.make[IO].map(Some(_)) else IO.pure(Option.empty[RunDiagnostics[IO]])
      backtests = List.tabulate(foldCount) { fold => (indicator: Indicator) =>
        calls
          .update(counts => counts.updated((indicator, fold), counts.getOrElse((indicator, fold), 0) + 1))
          .as(List(OrderStats(totalProfit = BigDecimal(foldScore(indicator, fold)))))
      }
      evaluator <- FoldRotatingEvaluator.cached[IO](backtests, scoring, space.canonicalise, observer = diagnostics.map(_.evaluatorObserver))
      validationObjective = (indicator: Indicator) =>
        validations.update(_ :+ indicator).as(Fitness(200.0 - math.abs(length(indicator) - 45)))
      delegate = new Tracker[IO, Indicator] {
        override def displayInitial(target: Indicator, params: Parameters[?]): IO[Unit] = IO.unit
        override def displayProgress(progress: Progress[Indicator]): IO[Unit]           = IO.unit
        override def displayFinal(population: ValidatedPopulation[Indicator]): IO[Unit] = IO.unit
        override def displayNote(title: String, lines: List[String]): IO[Unit]          = notes.update(_ :+ title)
      }
      tracker = diagnostics.fold[Tracker[IO, Indicator]](delegate) { observed =>
        new ReportingTracker(delegate, observed, foldCount, logInterval = 1)
      }
      population <- params match {
        case ga: Parameters.GA =>
          for {
            selector   <- Selector.tournament[IO, Indicator]
            elitism    <- Elitism.simple[IO, Indicator]
            validator  <- Validator.shortlisted[IO, Indicator](4, validationObjective)
            population <- OptimisationAlgorithm
              .ga[IO, Indicator](
                search.initialiser,
                search.crossover,
                search.mutator,
                evaluator,
                validator,
                selector,
                elitism,
                tracker
              )
              .optimise(target, ga)
          } yield population
        case scga: Parameters.SCGA =>
          for {
            species   <- SpeciesOperators.make[IO, Indicator](IndicatorDistance.make(space))
            validator <- Validator.speciesShortlisted[IO, Indicator](
              4,
              species,
              scga.speciesRadius,
              scga.effectiveMaxSpecies,
              validationObjective
            )
            population <- OptimisationAlgorithm
              .scga[IO, Indicator](
                search.initialiser,
                search.crossover,
                search.mutator,
                evaluator,
                validator,
                species,
                tracker
              )
              .optimise(target, scga)
          } yield population
      }
      nextRandom <- IO(random.nextLong())
      foldCalls  <- calls.get
      validated  <- validations.get
      snapshot   <- diagnostics.traverse(_.snapshot)
      noteTitles <- notes.get
    } yield Outcome(population, nextRandom, foldCalls, validated, snapshot, noteTitles)
  }

  "Search reporting" should {
    val parameters: List[Parameters.GA | Parameters.SCGA] = List(
      gaParameters,
      Parameters.SCGA.from(gaParameters).copy(speciesRadius = 0.1, maxSpecies = 3, interspeciesMatingProbability = 0.15)
    )

    parameters.foreach { params =>
      s"preserve ${params.name} search results, random draws and backtest work" in {
        val measuredRun = run(params, instrumented = true)
        val result      = for {
          plain    <- run(params, instrumented = false)
          measured <- measuredRun
          repeated <- measuredRun
        } yield (plain, measured, repeated)

        result.asserting { case (plain, measured, repeated) =>
          measured.population mustBe plain.population
          measured.population must not be empty
          measured.nextRandom mustBe plain.nextRandom
          measured.foldCalls mustBe plain.foldCalls
          measured.validations mustBe plain.validations
          measured.foldCalls.values.toSet mustBe Set(1)
          measured.noteTitles.count(_.startsWith("Stable progress:")) mustBe 5
          plain.noteTitles mustBe empty
          repeated mustBe measured

          val snapshot = measured.snapshot.getOrElse(fail("Expected diagnostics from the measured run"))
          snapshot.firstSeen must not be empty
          snapshot.firstSeen.values.min mustBe 0
          snapshot.firstSeen.values.foreach { generation =>
            generation must be >= 0
            generation must be <= 5
          }
          val expectedBest = snapshot.firstSeen.toList
            .map { case (indicator, generation) => RunDiagnostics.Discovery(indicator, stableScore(indicator), generation) }
            .sortBy(discovery => (-discovery.fitness, discovery.generation, discovery.indicator.toString))
            .head
          snapshot.bestSeen mustBe Some(expectedBest)
          snapshot.searchRequests must be > 24L
          snapshot.rescoreRequests must be > 0L
          snapshot.cacheReuses must be > 0L
          snapshot.completedComputations mustBe measured.foldCalls.keys.map(_._1).toSet.size.toLong
          snapshot.workloads(RunDiagnostics.Stage.Search).foldCompleted mustBe measured.foldCalls.values.sum.toLong
        }
      }
    }
  }
}
