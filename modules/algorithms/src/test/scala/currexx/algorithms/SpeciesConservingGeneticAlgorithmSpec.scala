package currexx.algorithms

import cats.effect.{IO, Ref}
import cats.~>
import cats.syntax.all.*
import currexx.algorithms.operators.{Crossover, Evaluator, Initialiser, Mutator, Validator}
import currexx.algorithms.progress.{Progress, Tracker}
import currexx.algorithms.operators.species.{Distance, SpeciesOperators, SpeciesStats}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class SpeciesConservingGeneticAlgorithmSpec extends IOWordSpec {
  private val distance = new Distance[Double]:
    def between(a: Double, b: Double): Either[IllegalArgumentException, Double] = Right(math.abs(a - b))

  private case class Observation(
      result: ValidatedPopulation[Double],
      evaluated: Vector[(Double, EvaluationPhase)],
      mutated: Vector[Double],
      progress: Vector[Progress[Double]],
      validations: Int,
      validationCandidates: Vector[Double],
      operations: Vector[String],
      breedingPopulations: Vector[Population[Double]]
  )

  private def run(
      params: Parameters.SCGA,
      initial: Vector[Double] = Vector(0.0, 10.0),
      fitness: (Double, EvaluationPhase) => Double = (value, _) => value,
      mutation: Double => Double = identity
  ): IO[Observation] = {
    given Random = Random(42)
    for {
      evaluated            <- Ref.of[IO, Vector[(Double, EvaluationPhase)]](Vector.empty)
      mutated              <- Ref.of[IO, Vector[Double]](Vector.empty)
      progress             <- Ref.of[IO, Vector[Progress[Double]]](Vector.empty)
      validations          <- Ref.of[IO, Int](0)
      validationCandidates <- Ref.of[IO, Vector[Double]](Vector.empty)
      operations           <- Ref.of[IO, Vector[String]](Vector.empty)
      breedingPopulations  <- Ref.of[IO, Vector[Population[Double]]](Vector.empty)
      initialiser          <- Initialiser.custom[IO, Double]((_, size, _) => IO.pure(Vector.tabulate(size)(i => initial(i % initial.size))))
      crossover = new Crossover[IO, Double] {
        def cross(a: Double, b: Double)(using Random): IO[Double]                      = IO.pure(a)
        def cross(a: Double, b: Double, probability: Double)(using Random): IO[Double] = IO.pure(a)
      }
      mutator = new Mutator[IO, Double] {
        def mutate(individual: Double, probability: Double)(using Random): IO[Double] =
          mutated.update(_ :+ individual).as(mutation(individual))
      }
      evaluator = new Evaluator[IO, Double] {
        def evaluateIndividual(individual: Double, phase: EvaluationPhase): IO[(Double, Fitness)] =
          evaluated.update(_ :+ (individual -> phase)).as(individual -> Fitness(fitness(individual, phase)))
      }
      validationObjective = (individual: Double) => validationCandidates.update(_ :+ individual).as(Fitness(individual + 1000.0))
      validator           = new Validator[IO, Double] {
        def validate(population: EvaluatedPopulation[Double]): IO[ValidatedPopulation[Double]] =
          validations.update(_ + 1) *> population.toList
            .traverse { case (individual, score) =>
              validationObjective(individual).map(validation => (individual, score, validation))
            }
            .map(_.toVector)
      }
      tracker = new Tracker[IO, Double] {
        def displayInitial(target: Double, parameters: Parameters[?]): IO[Unit] = IO.unit
        def displayProgress(event: Progress[Double]): IO[Unit]                  = progress.update(_ :+ event)
        def displayFinal(population: ValidatedPopulation[Double]): IO[Unit]     = IO.unit
        def displayNote(title: String, lines: List[String]): IO[Unit]           = IO.unit
      }
      species <- SpeciesOperators.make[IO, Double](distance)
      baseInterpreter      = Op.scgaInterpreter(initialiser, crossover, mutator, evaluator, validator, species, tracker)
      recordingInterpreter = new (Op[*, Double] ~> IO) {
        def apply[A](operation: Op[A, Double]): IO[A] = {
          val name = operation match {
            case Op.EvaluatePopulation(_, phase) => s"EvaluatePopulation($phase)"
            case _                               => operation.productPrefix
          }
          val recordBreeding = operation match {
            case Op.SelectSpeciesPairs(groups, _, _) => breedingPopulations.update(_ :+ groups.population)
            case _                                   => IO.unit
          }
          operations.update(_ :+ name) *> recordBreeding *> baseInterpreter(operation)
        }
      }
      result       <- Algorithm.SCGA.optimise(0.0, params).foldMap(recordingInterpreter)
      observations <- (
        evaluated.get,
        mutated.get,
        progress.get,
        validations.get,
        validationCandidates.get,
        operations.get,
        breedingPopulations.get
      )
        .mapN(Observation(result, _, _, _, _, _, _, _))
    } yield observations
  }

  private val params = Parameters.SCGA(4, 1, 0.0, 0.0, shuffle = true, speciesRadius = 1.0, maxSpecies = 2)

  "Algorithm.SCGA" should {
    "protect a distant representative when truncating the initial oversampled population" in
      run(
        params.copy(maxGen = 0, initialOversampling = 3, speciesRadius = 20.0),
        Vector.tabulate(11)(_.toDouble) :+ 1000.0,
        (value, _) => -value
      ).asserting { observed =>
        observed.result.map(_._1) mustBe Vector(0.0, 1.0, 2.0, 1000.0)
        observed.validations mustBe 1
        observed.mutated mustBe empty
        observed.progress mustBe empty
        observed.breedingPopulations mustBe empty
        observed.operations mustBe Vector(
          "DisplayInitial",
          "InitPopulation",
          "EvaluatePopulation(Search(0))",
          "IdentifySpecies",
          "ConserveSpecies",
          "EvaluatePopulation(Rescore)",
          "SortByFitness",
          "ValidatePopulation",
          "DisplayFinal"
        )
      }

    "identify and conserve the initial species once before breeding only their retained members" in
      run(
        params.copy(maxGen = 2, initialOversampling = 3, speciesRadius = 20.0),
        Vector.tabulate(11)(_.toDouble) :+ 1000.0,
        (value, _) => -value
      ).asserting { observed =>
        val retained = Set(0.0, 1.0, 2.0, 1000.0)
        observed.breedingPopulations.head.sorted mustBe Vector(0.0, 1.0, 2.0, 1000.0)
        observed.breedingPopulations.foreach { population =>
          population.size mustBe 4
          population.forall(retained.contains) mustBe true
        }
        observed.mutated.forall(retained.contains) mustBe true
        observed.operations mustBe (
          Vector("DisplayInitial", "InitPopulation", "EvaluatePopulation(Search(0))", "IdentifySpecies", "ConserveSpecies") ++
            (1 to 2).toVector.flatMap { generation =>
              Vector(
                "SelectSpeciesPairs",
                "ApplyToAll",
                "ApplyToAll",
                s"EvaluatePopulation(Search($generation))",
                "SortByFitness",
                "DisplayProgress",
                "IdentifySpecies",
                "ConserveSpecies"
              )
            } ++
            Vector("EvaluatePopulation(Rescore)", "SortByFitness", "ValidatePopulation", "DisplayFinal")
        )
        observed.evaluated.count(_._2 == EvaluationPhase.Search(0)) mustBe 12
        observed.evaluated.count(_._2 == EvaluationPhase.Search(1)) mustBe 4
        observed.evaluated.count(_._2 == EvaluationPhase.Search(2)) mustBe 4
        observed.validations mustBe 1
      }

    "keep exactly the requested population, including odd and singleton sizes" in
      List(1, 2, 3, 5, 10).traverse(size => run(params.copy(populationSize = size, maxGen = 3)).map(size -> _)).asserting { runs =>
        runs.foreach { case (size, observed) =>
          observed.result must have size size
          observed.progress.map(event => (event.currentGen, event.maxGen)) mustBe Vector((1, 3), (2, 3), (3, 3))
          observed.progress.foreach {
            case Progress.Species(_, _, population, _) => population must have size size
            case other                                 => fail(s"Expected species progress, got $other")
          }
          observed.validations mustBe 1
          observed.validationCandidates must have size size
          (0 to 3).foreach { generation =>
            observed.evaluated.count(_._2 == EvaluationPhase.Search(generation)) mustBe size
          }
          observed.evaluated.count(_._2 == EvaluationPhase.Rescore) mustBe size
        }
        succeed
      }

    "keep representatives untouched and report completed populations with their breeding-parent statistics" in
      run(params.copy(maxGen = 2, interspeciesMatingProbability = 0.0), mutation = _ + 1.0).asserting { observed =>
        observed.result.map(_._1).toSet mustBe Set(1.0, 2.0, 11.0, 12.0)
        observed.mutated must have size 4
        observed.progress mustBe Vector(
          Progress
            .Species(1, 2, Vector(11.0, 10.0, 1.0, 0.0).map(value => value -> Fitness(value)), SpeciesStats(Vector(2, 2), Vector(1, 1), 2)),
          Progress.Species(
            2,
            2,
            Vector(12.0, 11.0, 2.0, 1.0).map(value => value -> Fitness(value)),
            SpeciesStats(Vector(2, 2), Vector(1, 1), 4)
          )
        )
      }

    "select from stored fitness and evaluate the next population once per generation" in {
      val phaseDependent: (Double, EvaluationPhase) => Double = (value, phase) =>
        phase match
          case EvaluationPhase.Search(0) => if (value == 0.0) 100.0 else 0.0
          case EvaluationPhase.Search(_) => if (value == 1.0) 100.0 else 0.0
          case EvaluationPhase.Rescore   => value
      run(params.copy(populationSize = 2, speciesRadius = 2.0), Vector(0.0, 1.0), phaseDependent, _ => 5.0).asserting { observed =>
        observed.result.map(_._1) mustBe Vector(5.0, 0.0)
        observed.result.map { case (_, training, validation) => (training, validation) } mustBe
          Vector((Fitness(5.0), Fitness(1005.0)), (Fitness(0.0), Fitness(1000.0)))
        observed.validationCandidates mustBe Vector(5.0, 0.0)
        observed.mutated mustBe Vector(0.0)
        observed.evaluated.filter(_._2 == EvaluationPhase.Search(1)).map(_._1).sorted mustBe Vector(0.0, 5.0)
        observed.progress.head.population.map(_._2.value) mustBe Vector(0.0, 0.0)
        observed.evaluated.takeRight(2).map(_._2) mustBe Vector.fill(2)(EvaluationPhase.Rescore)
        observed.validations mustBe 1
      }
    }

    "produce repeatable results with the same random seed" in
      (run(params.copy(maxGen = 4)), run(params.copy(maxGen = 4))).tupled.asserting { case (first, second) =>
        first.result mustBe second.result
        first.mutated mustBe second.mutated
      }
  }

  "Parameters.SCGA" should {
    "copy GA budgets and variation settings without copying global elitism" in {
      val ga = Parameters.GA(301, 17, 0.6, 0.2, 0.03, shuffle = true, initialOversampling = 3)
      Parameters.SCGA.from(ga) mustBe Parameters.SCGA(301, 17, 0.6, 0.2, shuffle = true, initialOversampling = 3)
      params.copy(populationSize = 3, maxSpecies = 8).effectiveMaxSpecies mustBe 1
      params.copy(populationSize = 1).effectiveMaxSpecies mustBe 1
    }

  }
}
