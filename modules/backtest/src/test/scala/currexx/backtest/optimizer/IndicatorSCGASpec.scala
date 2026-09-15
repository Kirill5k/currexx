package currexx.backtest.optimizer

import cats.effect.{IO, Ref}
import currexx.algorithms.operators.{Evaluator, Validator}
import currexx.algorithms.operators.species.SpeciesOperators
import currexx.algorithms.progress.{Progress, Tracker}
import currexx.algorithms.{EvaluatedPopulation, EvaluationPhase, Fitness, Parameters, ValidatedPopulation}
import currexx.backtest.TestStrategy
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class IndicatorSCGASpec extends IOWordSpec {
  private def candidate(length: Int, rawLength: Int = 1, momentumLength: Int = 12): Indicator =
    Indicator.compositeAllOf(
      Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(length)),
      Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(rawLength)),
      Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(momentumLength))
    )

  private val template = candidate(20)
  private val params   = Parameters.SCGA(
    populationSize = 9,
    maxGen = 3,
    crossoverProbability = 0.85,
    mutationProbability = 0.6,
    shuffle = true,
    initialOversampling = 3,
    maxSpecies = 3
  )

  private def searchSpace(fixed: Set[Indicator] = Set.empty): IndicatorSearchSpace =
    IndicatorSearchSpace.forStrategy(TestStrategy(template, TradeStrategy(Nil, Nil)), fixed).fold(error => fail(error.getMessage), identity)

  private def score(indicator: Indicator): Fitness = indicator match
    case Indicator.Composite(children, _) =>
      children.head match
        case Indicator.TrendChangeDetection(_, VT.SMA(length)) => Fitness(1.0 / (1.0 + math.abs(length - 40)))
        case other                                             => fail(s"Unexpected trend in test: $other")
    case other => fail(s"Unexpected indicator in test: $other")

  private def recordingTracker(progress: Ref[IO, Vector[EvaluatedPopulation[Indicator]]]): Tracker[IO, Indicator] =
    new Tracker[IO, Indicator] {
      override def displayInitial(target: Indicator, params: Parameters[?]): IO[Unit] = IO.unit
      override def displayProgress(event: Progress[Indicator]): IO[Unit]              =
        progress.update(_ :+ event.population)
      override def displayFinal(population: ValidatedPopulation[Indicator]): IO[Unit] = IO.unit
      override def displayNote(title: String, lines: List[String]): IO[Unit]          = IO.unit
    }

  "The indicator SCGA integration" should {
    "evolve compatible indicators at an exact population budget and validate only canonical distinct finalists" in {
      given Random   = Random(418)
      val space      = searchSpace()
      val fixedAlias = candidate(20, rawLength = 99, momentumLength = 45)
      val otherSeed  = candidate(80, rawLength = 55, momentumLength = 30)

      val result = for
        evaluated       <- Ref.of[IO, Vector[(Indicator, EvaluationPhase)]](Vector.empty)
        progress        <- Ref.of[IO, Vector[EvaluatedPopulation[Indicator]]](Vector.empty)
        events          <- Ref.of[IO, Vector[String]](Vector.empty)
        validationCalls <- Ref.of[IO, Int](0)
        operators       <- IndicatorSearchOperators.make[IO](space, List(fixedAlias, otherSeed))
        species         <- SpeciesOperators.make[IO, Indicator](IndicatorDistance.make(space))
        evaluator = new Evaluator[IO, Indicator] {
          override def evaluateIndividual(individual: Indicator, phase: EvaluationPhase): IO[(Indicator, Fitness)] =
            evaluated.update(_ :+ (individual -> phase)) *> events.update(_ :+ s"evaluate:$phase") *> IO.pure(
              individual -> score(individual)
            )
        }
        shortlist <- Validator.speciesShortlisted[IO, Indicator](
          4,
          species,
          params.speciesRadius,
          params.effectiveMaxSpecies,
          individual => events.update(_ :+ "validate") *> IO.pure(score(individual))
        )
        validator = new Validator[IO, Indicator] {
          override def validate(population: EvaluatedPopulation[Indicator]): IO[ValidatedPopulation[Indicator]] =
            validationCalls.update(_ + 1) *> shortlist.validate(population)
        }
        algorithm = OptimisationAlgorithm.scga(
          operators.initialiser,
          operators.crossover,
          operators.mutator,
          evaluator,
          validator,
          species,
          recordingTracker(progress)
        )
        finalists    <- algorithm.optimise(template, params)
        observations <- evaluated.get
        generations  <- progress.get
        chronology   <- events.get
        calls        <- validationCalls.get
      yield (finalists, observations, generations, chronology, calls)

      result.asserting { case (finalists, observations, generations, chronology, calls) =>
        generations must have size params.maxGen
        generations.foreach(_ must have size params.populationSize)
        observations.count(_._2 == EvaluationPhase.Search(0)) mustBe params.populationSize * params.initialOversampling
        observations.count(_._2 == EvaluationPhase.Rescore) mustBe params.populationSize
        observations.map(_._1).distinct.size must be > 3
        observations.foreach { case (individual, _) =>
          space.accepts(individual) mustBe true
          space.canonicalise(individual) mustBe Right(individual)
          individual match
            case Indicator.Composite(children, _) =>
              children.toList.drop(1) mustBe List(
                Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1)),
                Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(12))
              )
            case other => fail(s"Lost indicator structure: $other")
        }
        finalists must not be empty
        finalists.size must be <= 4
        finalists.map(_._1).distinct.size mustBe finalists.size
        calls mustBe 1
        chronology.takeRight(finalists.size) mustBe Vector.fill(finalists.size)("validate")
        chronology.dropRight(finalists.size).forall(_.startsWith("evaluate:")) mustBe true
      }
    }

    "conserve an entirely fixed population and spend one validation slot on fixed-only seed aliases" in {
      given Random = Random(721)
      val space    = searchSpace(Set(template))
      val aliases  = List(candidate(99, 45, 40), candidate(5, 88, 7))

      val result = for
        progress  <- Ref.of[IO, Vector[EvaluatedPopulation[Indicator]]](Vector.empty)
        validated <- Ref.of[IO, Vector[Indicator]](Vector.empty)
        operators <- IndicatorSearchOperators.make[IO](space, aliases)
        species   <- SpeciesOperators.make[IO, Indicator](IndicatorDistance.make(space))
        evaluator <- Evaluator.cached[IO, Indicator](individual => IO.pure(individual -> score(individual)))
        validator <- Validator.speciesShortlisted[IO, Indicator](
          4,
          species,
          params.speciesRadius,
          params.effectiveMaxSpecies,
          individual => validated.update(_ :+ individual) *> IO.pure(score(individual))
        )
        algorithm = OptimisationAlgorithm.scga(
          operators.initialiser,
          operators.crossover,
          operators.mutator,
          evaluator,
          validator,
          species,
          recordingTracker(progress)
        )
        finalists        <- algorithm.optimise(aliases.head, params)
        generations      <- progress.get
        validationInputs <- validated.get
      yield (finalists, generations, validationInputs)

      result.asserting { case (finalists, generations, validationInputs) =>
        generations must have size params.maxGen
        generations.foreach(_.map(_._1) mustBe Vector.fill(params.populationSize)(template))
        validationInputs mustBe Vector(template)
        finalists mustBe Vector((template, score(template), score(template)))
      }
    }
  }
}
