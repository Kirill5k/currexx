package currexx.algorithms

import cats.effect.{IO, Ref}
import cats.syntax.traverse.*
import currexx.algorithms.operators.{Crossover, Elitism, Evaluator, Initialiser, Mutator, Selector, Validator}
import currexx.algorithms.progress.{Progress, Tracker}
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class GeneticAlgorithmPopulationSpec extends IOWordSpec {
  private case class Observation(
      result: ValidatedPopulation[Int],
      evaluated: Vector[(Int, EvaluationPhase)],
      mutated: Vector[Int],
      progress: Vector[Progress[Int]]
  )

  private val parameters = Parameters.GA(5, 3, 0.7, 0.1, 0.4, shuffle = true)

  private def run(
      params: Parameters.GA,
      initial: Vector[Int] = Vector(0, 1),
      fitness: (Int, EvaluationPhase) => Double = (value, _) => value.toDouble,
      mutation: Int => Int = identity,
      roulette: Boolean = false
  ): IO[Observation] = {
    given Random = Random(42)
    for {
      evaluated   <- Ref.of[IO, Vector[(Int, EvaluationPhase)]](Vector.empty)
      mutated     <- Ref.of[IO, Vector[Int]](Vector.empty)
      progress    <- Ref.of[IO, Vector[Progress[Int]]](Vector.empty)
      initialiser <- Initialiser.custom[IO, Int]((_, size, _) => IO.pure(Vector.tabulate(size)(index => initial(index % initial.size))))
      crossover = new Crossover[IO, Int] {
        def cross(a: Int, b: Int)(using Random): IO[Int]                      = IO.pure(a)
        def cross(a: Int, b: Int, probability: Double)(using Random): IO[Int] = IO.pure(a)
      }
      mutator = new Mutator[IO, Int] {
        def mutate(individual: Int, probability: Double)(using Random): IO[Int] =
          mutated.update(_ :+ individual).as(mutation(individual))
      }
      evaluator = new Evaluator[IO, Int] {
        def evaluateIndividual(individual: Int, phase: EvaluationPhase): IO[(Int, Fitness)] =
          evaluated.update(_ :+ (individual -> phase)).as(individual -> Fitness(fitness(individual, phase)))
      }
      tracker = new Tracker[IO, Int] {
        def displayInitial(target: Int, params: Parameters[?]): IO[Unit] = IO.unit
        def displayProgress(event: Progress[Int]): IO[Unit]              = progress.update(_ :+ event)
        def displayFinal(population: ValidatedPopulation[Int]): IO[Unit] = IO.unit
        def displayNote(title: String, lines: List[String]): IO[Unit]    = IO.unit
      }
      validator <- Validator.none[IO, Int]
      selector  <- if (roulette) Selector.rouletteWheel[IO, Int] else Selector.tournament[IO, Int]
      elitism   <- Elitism.simple[IO, Int]
      result    <- Algorithm.GA
        .optimise(0, params)
        .foldMap(Op.ioInterpreter(initialiser, crossover, mutator, evaluator, validator, selector, elitism, tracker))
      evaluations <- evaluated.get
      mutations   <- mutated.get
      generations <- progress.get
    } yield Observation(result, evaluations, mutations, generations)
  }

  "Algorithm.GA population budget" should {
    "include elites within the requested size for odd, even and singleton populations with either selector" in {
      val configurations = List(1, 2, 3, 5, 10).flatMap { size =>
        List(0.0, 0.25, 1.0).map(ratio => parameters.copy(populationSize = size, elitismRatio = ratio))
      } :+ parameters.copy(populationSize = 300, elitismRatio = 0.02)
      val cases = configurations.flatMap(params => List(false, true).map(roulette => (params, roulette)))

      cases.traverse { case (params, roulette) => run(params, roulette = roulette).map(params -> _) }.asserting { runs =>
        runs.foreach { case (params, observed) =>
          observed.result must have size params.populationSize
          observed.progress must have size params.maxGen
          observed.progress.foreach(_.population must have size params.populationSize)
          (0 to params.maxGen).foreach { generation =>
            observed.evaluated.count(_._2 == EvaluationPhase.Search(generation)) mustBe params.populationSize
          }
          observed.evaluated.count(_._2 == EvaluationPhase.Rescore) mustBe params.populationSize
          val eliteCount = (params.populationSize * params.elitismRatio).toInt
          observed.mutated must have size (params.maxGen * (params.populationSize - eliteCount))
        }
        succeed
      }
    }

    "preserve elites unchanged while mutating only the remaining population slots" in
      run(parameters.copy(maxGen = 1), Vector.range(0, 5), mutation = _ => -1).asserting { observed =>
        observed.progress.head.population.map(_._1) mustBe Vector(4, 3, -1, -1, -1)
        observed.mutated must have size 3
        observed.evaluated.filter(_._2 == EvaluationPhase.Search(1)).map(_._1).sorted mustBe Vector(-1, -1, -1, 3, 4)
      }

    "reevaluate elites and offspring under the same generation after the objective changes" in {
      val phaseDependent: (Int, EvaluationPhase) => Double = (value, phase) =>
        phase match
          case EvaluationPhase.Search(0) => if (value == 0) 100.0 else 0.0
          case EvaluationPhase.Search(_) => if (value == 5) 10.0 else 0.0
          case EvaluationPhase.Rescore   => value.toDouble

      run(
        parameters.copy(populationSize = 2, maxGen = 1, elitismRatio = 0.5),
        fitness = phaseDependent,
        mutation = _ => 5
      ).asserting { observed =>
        observed.progress.head.population mustBe Vector(5 -> Fitness(10.0), 0 -> Fitness(0.0))
        observed.evaluated.filter(_._2 == EvaluationPhase.Search(1)).map(_._1).sorted mustBe Vector(0, 5)
        observed.result.map { case (value, training, _) => value -> training } mustBe Vector(5 -> Fitness(5.0), 0 -> Fitness(0.0))
      }
    }

    "oversample only the initial draw and keep the best initial elites within the subsequent budget" in {
      val params = parameters.copy(maxGen = 2, initialOversampling = 3)
      run(params, Vector.range(0, 15), mutation = _ => -1).asserting { observed =>
        observed.evaluated.filter(_._2 == EvaluationPhase.Search(0)).map(_._1).sorted mustBe Vector.range(0, 15)
        observed.progress.foreach { generation =>
          generation.population.map(_._1) mustBe Vector(14, 13, -1, -1, -1)
        }
        (1 to params.maxGen).foreach { generation =>
          observed.evaluated.count(_._2 == EvaluationPhase.Search(generation)) mustBe params.populationSize
        }
        observed.evaluated.count(_._2 == EvaluationPhase.Rescore) mustBe params.populationSize
        observed.mutated must have size 6
        observed.result must have size params.populationSize
      }
    }
  }
}
