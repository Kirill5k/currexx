package currexx.algorithms

import cats.effect.IO
import cats.syntax.all.*
import cats.~>
import currexx.algorithms.operators.*
import currexx.algorithms.operators.species.*
import currexx.algorithms.progress.Tracker
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

class OpInterpreterSpec extends IOWordSpec {
  private val population: EvaluatedPopulation[Int] = Vector(10 -> Fitness(10.0), 0 -> Fitness(0.0))

  private def interpreters: IO[(Op[*, Int] ~> IO, Op[*, Int] ~> IO)] = {
    given Random = Random(42)
    val distance = new Distance[Int] {
      override def between(a: Int, b: Int): Either[IllegalArgumentException, Double] = Right(math.abs(a.toDouble - b))
    }
    val crossover = new Crossover[IO, Int] {
      override def cross(a: Int, b: Int)(using Random): IO[Int] = IO.pure(a)
      override def cross(a: Int, b: Int, probability: Double)(using Random): IO[Int] = IO.pure(a)
    }
    val mutator = new Mutator[IO, Int] {
      override def mutate(individual: Int, probability: Double)(using Random): IO[Int] = IO.pure(individual)
    }
    for
      initialiser <- Initialiser.simple[IO, Int](IO.pure)
      evaluator <- Evaluator.cached[IO, Int](individual => IO.pure(individual -> Fitness(individual.toDouble)))
      validator <- Validator.none[IO, Int]
      selector <- Selector.tournament[IO, Int]
      elitism <- Elitism.simple[IO, Int]
      species <- SpeciesOperators.make[IO, Int](distance)
      tracker <- Tracker.noop[IO, Int]
    yield (
      Op.ioInterpreter(initialiser, crossover, mutator, evaluator, validator, selector, elitism, tracker),
      Op.scgaInterpreter(initialiser, crossover, mutator, evaluator, validator, species, tracker)
    )
  }

  "The algorithm interpreters" should {
    "dispatch GA elitism and recurse through GA selection inside ApplyToAll" in {
      interpreters.flatMap { case (ga, _) =>
        for
          elites <- ga(Op.SelectElites(population, 4, 0.25))
          pairs <- ga(Op.ApplyToAll(Vector(2, 4), (limit: Int) => Op.SelectPairs(population, limit)))
        yield (elites, pairs)
      }.asserting { case (elites, pairs) =>
        elites mustBe Vector(10)
        pairs mustBe Vector(Vector((10, 10)), Vector((10, 10), (10, 10)))
      }
    }

    "dispatch speciation and recurse through SCGA conservation and breeding inside ApplyToAll" in {
      val candidates: EvaluatedPopulation[Int] = Vector(0 -> Fitness(10.0), 0 -> Fitness(9.0), 10 -> Fitness(2.0), 11 -> Fitness(1.0))
      interpreters.flatMap { case (_, scga) =>
        for
          groups <- scga(Op.IdentifySpecies(candidates, 1.0, 2))
          conserved <- scga(Op.ApplyToAll(Vector(2, 3), (size: Int) => Op.ConserveSpecies(groups, size)))
          breeding <- scga(Op.ApplyToAll(conserved, (retained: SpeciesPopulation[Int]) => Op.SelectSpeciesPairs(retained, 4, 0.0)))
        yield (groups, conserved, breeding)
      }.asserting { case (groups, conserved, breeding) =>
        groups.sizes mustBe Vector(2, 2)
        groups.representatives mustBe Vector(0, 10)
        conserved.map(_.population) mustBe Vector(Vector(0, 10), Vector(0, 0, 10))
        breeding mustBe Vector.fill(2)(SpeciesBreeding(Vector((0, 0), (10, 10)), Vector(1, 1)))
      }
    }

    "reject every operation belonging to the other algorithm through the returned effect" in {
      val groups = SpeciesPopulation(Vector(Species(population.head, population)))
      interpreters.flatMap { case (ga, scga) =>
        for
          // A synchronous throw here fails construction instead of becoming one of the expected effect failures below.
          effects <- IO {
            List(
              "GA interpreter does not support IdentifySpecies" -> ga(Op.IdentifySpecies(population, 1.0, 2)).void,
              "GA interpreter does not support ConserveSpecies" -> ga(Op.ConserveSpecies(groups, 2)).void,
              "GA interpreter does not support SelectSpeciesPairs" -> ga(Op.SelectSpeciesPairs(groups, 2, 0.0)).void,
              "SCGA interpreter does not support SelectElites" -> scga(Op.SelectElites(population, 2, 0.5)).void,
              "SCGA interpreter does not support SelectPairs" -> scga(Op.SelectPairs(population, 2)).void
            )
          }
          outcomes <- effects.traverse { case (message, effect) => effect.attempt.map(message -> _) }
        yield outcomes
      }.asserting { outcomes =>
        outcomes.foreach {
          case (message, Left(error: IllegalStateException)) => error.getMessage mustBe message
          case other => fail(s"Expected an effect containing the algorithm-specific unsupported-operation error, got $other")
        }
        succeed
      }
    }
  }
}
