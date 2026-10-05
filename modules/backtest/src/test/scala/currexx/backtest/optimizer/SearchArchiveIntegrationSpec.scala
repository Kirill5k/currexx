package currexx.backtest.optimizer

import cats.effect.{IO, Ref}
import currexx.algorithms.{EvaluatedPopulation, EvaluationPhase, Fitness, Parameters, ValidatedPopulation}
import currexx.algorithms.operators.{Elitism, Evaluator, Selector, Validator}
import currexx.algorithms.operators.species.SpeciesOperators
import currexx.algorithms.progress.{Progress, Tracker}
import currexx.backtest.{OrderStats, TestStrategy}
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class SearchArchiveIntegrationSpec extends IOWordSpec {
  private val foldCount     = 3
  private val shortlistSize = 4
  private val target        = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(20))
  private val strategy      = TestStrategy(target, TradeStrategy(Nil, Nil))
  private val gaParameters  = Parameters.GA(
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

  private def period(indicator: Indicator): Int = indicator match {
    case Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(n)) => n
    case other                                                        => throw new IllegalArgumentException(s"Unexpected candidate: $other")
  }

  private def foldScore(indicator: Indicator, fold: Int): Double =
    300.0 - math.abs(period(indicator) - (25 + fold * 30))

  private type FoldCalls = Map[(Indicator, Int), Int]

  final private case class Boundary(population: EvaluatedPopulation[Indicator], foldCalls: FoldCalls)

  final private case class Outcome(
      progress: Vector[Progress[Indicator]],
      boundary: Boundary,
      finalists: ValidatedPopulation[Indicator],
      nextRandom: Long,
      foldCalls: FoldCalls,
      validated: Vector[Indicator],
      events: Vector[String]
  )

  private def run(params: Parameters.GA | Parameters.SCGA, preserveCandidates: Boolean): IO[Outcome] = IO.defer {
    given random: Random = Random(42)
    for {
      space       <- IO.fromEither(IndicatorSearchSpace.forStrategy(strategy))
      baselines   <- IO.fromEither(FinalistAssembler.protectedBaselines(space, Nil, shortlistSize))
      search      <- IndicatorSearchOperators.make[IO](space)
      archive     <- SearchArchive.make[IO](shortlistSize)
      calls       <- Ref.of[IO, FoldCalls](Map.empty)
      validations <- Ref.of[IO, Vector[Indicator]](Vector.empty)
      progress    <- Ref.of[IO, Vector[Progress[Indicator]]](Vector.empty)
      boundary    <- Ref.of[IO, Option[Boundary]](None)
      events      <- Ref.of[IO, Vector[String]](Vector.empty)
      backtests = List.tabulate(foldCount) { fold => (indicator: Indicator) =>
        events.update(_ :+ "fold") >> calls
          .update(counts => counts.updated((indicator, fold), counts.getOrElse((indicator, fold), 0) + 1))
          .as(List(OrderStats(totalProfit = BigDecimal(foldScore(indicator, fold)))))
      }
      cached <- FoldRotatingEvaluator.cached[IO](backtests, scoring, space.canonicalise)
      observed  = if (preserveCandidates) cached.observingSearch(archive.record) else cached
      evaluator = new Evaluator[IO, Indicator] {
        override def evaluateIndividual(indicator: Indicator, phase: EvaluationPhase): IO[(Indicator, Fitness)] =
          events.update(_ :+ "evaluation") >> observed.evaluateIndividual(indicator, phase)
      }
      validationObjective = (indicator: Indicator) =>
        events.update(_ :+ "validation") >> validations.update(_ :+ indicator).as(Fitness(200.0 - math.abs(period(indicator) - 45)))
      tracker = new Tracker[IO, Indicator] {
        override def displayInitial(target: Indicator, params: Parameters[?]): IO[Unit] = IO.unit
        override def displayProgress(value: Progress[Indicator]): IO[Unit]              = progress.update(_ :+ value)
        override def displayFinal(population: ValidatedPopulation[Indicator]): IO[Unit] = IO.unit
        override def displayNote(title: String, lines: List[String]): IO[Unit]          = IO.unit
      }
      captureBoundary = (delegate: Validator[IO, Indicator]) =>
        new Validator[IO, Indicator] {
          override def validate(population: EvaluatedPopulation[Indicator]): IO[ValidatedPopulation[Indicator]] =
            calls.get.flatMap(recorded => boundary.set(Some(Boundary(population, recorded)))) >>
              events.update(_ :+ "boundary") >> delegate.validate(population)
        }
      finalists <- params match {
        case ga: Parameters.GA =>
          for {
            selector <- Selector.tournament[IO, Indicator]
            elitism  <- Elitism.simple[IO, Indicator]
            ordinary <- Validator.shortlisted[IO, Indicator](shortlistSize, validationObjective)
            assembler = FinalistAssembler.ga(space, baselines, cached.evaluateIndividual(_, EvaluationPhase.Rescore))
            selected  = if (preserveCandidates) OptimisationAlgorithm.assemblingValidator(archive, assembler, ordinary) else ordinary
            result <- OptimisationAlgorithm
              .ga(search.initialiser, search.crossover, search.mutator, evaluator, captureBoundary(selected), selector, elitism, tracker)
              .optimise(target, ga)
          } yield result
        case scga: Parameters.SCGA =>
          val distance = IndicatorDistance.make(space)
          for {
            species  <- SpeciesOperators.make[IO, Indicator](distance)
            ordinary <- Validator.speciesShortlisted[IO, Indicator](
              shortlistSize,
              species,
              scga.speciesRadius,
              scga.effectiveMaxSpecies,
              validationObjective
            )
            shortlisted <- Validator.shortlisted[IO, Indicator](shortlistSize, validationObjective)
            assembler = FinalistAssembler.scga(
              space,
              baselines,
              cached.evaluateIndividual(_, EvaluationPhase.Rescore),
              species.speciation,
              distance,
              scga.speciesRadius,
              scga.effectiveMaxSpecies
            )
            selected = if (preserveCandidates) OptimisationAlgorithm.assemblingValidator(archive, assembler, shortlisted) else ordinary
            result <- OptimisationAlgorithm
              .scga(search.initialiser, search.crossover, search.mutator, evaluator, captureBoundary(selected), species, tracker)
              .optimise(target, scga)
          } yield result
      }
      displayed <- progress.get
      captured  <- boundary.get.flatMap(value =>
        IO.fromOption(value)(new IllegalStateException("Final validation boundary was not reached"))
      )
      nextRandom <- IO(random.nextLong())
      foldCalls  <- calls.get
      validated  <- validations.get
      chronology <- events.get
    } yield Outcome(displayed, captured, finalists, nextRandom, foldCalls, validated, chronology)
  }

  "Search archives and baseline-preserving assembly" should {
    val parameters: List[Parameters.GA | Parameters.SCGA] = List(
      gaParameters,
      Parameters.SCGA.from(gaParameters).copy(speciesRadius = 0.1, maxSpecies = 3, interspeciesMatingProbability = 0.15),
      gaParameters.copy(maxGen = 0),
      Parameters.SCGA.from(gaParameters).copy(maxGen = 0, speciesRadius = 0.1, maxSpecies = 3, interspeciesMatingProbability = 0.15)
    )

    parameters.foreach { params =>
      val generationCount = params match {
        case ga: Parameters.GA     => ga.maxGen
        case scga: Parameters.SCGA => scga.maxGen
      }
      s"preserve ${params.name} evolution and random draws with $generationCount generations" in {
        val result = for {
          ordinary <- run(params, preserveCandidates = false)
          archived <- run(params, preserveCandidates = true)
        } yield (ordinary, archived)

        result.asserting { case (ordinary, archived) =>
          archived.progress mustBe ordinary.progress
          archived.progress.size mustBe generationCount
          archived.boundary mustBe ordinary.boundary
          archived.nextRandom mustBe ordinary.nextRandom
          archived.boundary.foldCalls.keys.map(_._1).toSet must contain(target)
          archived.foldCalls mustBe archived.boundary.foldCalls
          archived.foldCalls mustBe ordinary.foldCalls
          archived.foldCalls.values.toSet mustBe Set(1)
          archived.validated must contain(target)
          archived.validated.distinct.size mustBe shortlistSize
          archived.validated.size mustBe shortlistSize
          archived.finalists.map(_._1).toSet mustBe archived.validated.toSet
          val boundaryIndex = archived.events.indexOf("boundary")
          boundaryIndex must be > 0
          archived.events.take(boundaryIndex) must not contain "validation"
          archived.events.drop(boundaryIndex + 1) mustBe Vector.fill(shortlistSize)("validation")
        }
      }
    }
  }
}
