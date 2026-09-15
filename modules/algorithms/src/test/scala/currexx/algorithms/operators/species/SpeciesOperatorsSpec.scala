package currexx.algorithms.operators.species

import cats.effect.IO
import cats.syntax.apply.*
import cats.syntax.traverse.*
import currexx.algorithms.{EvaluatedPopulation, Fitness}
import kirill5k.common.cats.test.IOWordSpec
import org.scalatest.EitherValues

import scala.util.Random

class SpeciesOperatorsSpec extends IOWordSpec with EitherValues {
  private val distance = new Distance[Double] {
    override def between(a: Double, b: Double): Either[IllegalArgumentException, Double] = Right(math.abs(a - b))
  }

  private def evaluated(members: (Double, Double)*): EvaluatedPopulation[Double] =
    members.toVector.map { case (individual, fitness) => (individual, Fitness(fitness)) }

  private def group(members: (Double, Double)*): Species[Double] =
    val population = evaluated(members*)
    Species(population.maxBy(_._2), population)

  private def singletons(scores: Double*): SpeciesPopulation[Int] =
    SpeciesPopulation(scores.toVector.zipWithIndex.map { case (score, index) =>
      val member = (index, Fitness(score))
      Species(member, Vector(member))
    })

  "Speciation" should {
    "seed from fitness order and include the radius boundary without making membership transitive" in {
      val population = evaluated(0.5 -> 2.0, 0.25 -> 3.0, 0.0 -> 4.0, 1.0 -> 1.0)
      Speciation.make[IO, Double](distance).flatMap(_.partition(population, 0.25, 8)).asserting { outcome =>
        val species = outcome.value.species
        species.map(_.representative._1) mustBe Vector(0.0, 0.5, 1.0)
        species.map(_.members.map(_._1)) mustBe Vector(Vector(0.0, 0.25), Vector(0.5), Vector(1.0))
        species.flatMap(_.members).sortBy(_._1) mustBe population.sortBy(_._1)
      }
    }

    "assign to the nearest seed after reaching the cap and break distance ties by creation order" in {
      val population = evaluated(10.0 -> 4.0, 0.0 -> 4.0, 5.0 -> 3.0, 1.0 -> 2.0)
      Speciation.make[IO, Double](distance).flatMap(_.partition(population, 0.5, 2)).asserting { outcome =>
        val species = outcome.value.species
        species.map(_.representative._1) mustBe Vector(10.0, 0.0)
        species.map(_.members.map(_._1)) mustBe Vector(Vector(10.0, 5.0), Vector(0.0, 1.0))
      }
    }

    "keep duplicate occurrences in one species even at zero radius" in {
      val population = evaluated(1.0 -> 2.0, 1.0 -> 2.0, 2.0 -> 1.0)
      Speciation.make[IO, Double](distance).flatMap(_.partition(population, 0.0, 8)).asserting { outcome =>
        val species = outcome.value.species
        species.map(_.members.size) mustBe Vector(2, 1)
        species.flatMap(_.members) mustBe population
      }
    }

    "return no species for an empty population" in
      Speciation
        .make[IO, Double](distance)
        .flatMap(_.partition(Vector.empty, 0.15, 8))
        .asserting(_ mustBe Right(SpeciesPopulation(Vector.empty)))

    "reject invalid radius and cap values" in
      Speciation
        .make[IO, Double](distance)
        .flatMap { speciation =>
          Vector(
            speciation.partition(Vector.empty, -0.1, 8),
            speciation.partition(Vector.empty, Double.NaN, 8),
            speciation.partition(Vector.empty, Double.PositiveInfinity, 8),
            speciation.partition(Vector.empty, 0.15, 0)
          ).sequence
        }
        .asserting { outcomes =>
          outcomes.map(_.left.value.getMessage) mustBe Vector(
            "Species radius must be finite and nonnegative",
            "Species radius must be finite and nonnegative",
            "Species radius must be finite and nonnegative",
            "Maximum species count must be positive"
          )
        }

    "reject nonfinite and negative distance results" in
      Vector(Double.NaN, Double.PositiveInfinity, -0.1)
        .traverse { invalid =>
          val invalidDistance = new Distance[Double] {
            override def between(a: Double, b: Double): Either[IllegalArgumentException, Double] = Right(invalid)
          }
          Speciation.make[IO, Double](invalidDistance).flatMap(_.partition(evaluated(0.0 -> 2.0, 1.0 -> 1.0), 0.15, 8))
        }
        .asserting(_.map(_.left.value.getMessage) mustBe Vector.fill(3)("Species distance must be finite and nonnegative"))

    "propagate a typed distance error without replacing it or raising an effect error" in {
      val problem         = new IllegalArgumentException("Candidate schema mismatch")
      val failingDistance = new Distance[Double] {
        override def between(a: Double, b: Double): Either[IllegalArgumentException, Double] = Left(problem)
      }
      Speciation
        .make[IO, Double](failingDistance)
        .flatMap(_.partition(evaluated(0.0 -> 2.0, 1.0 -> 1.0), 0.15, 8))
        .asserting(_ mustBe Left(problem))
    }
  }

  "SpeciesSelection" should {
    "reserve a representative and child per species before weighting the remaining slots by champion rank" in {
      given Random = Random(42)
      SpeciesSelection.make[IO, Int].flatMap(_.select(singletons(10.0, 5.0, 1.0), 20, 0.0)).asserting { outcome =>
        val result = outcome.value
        result.offspringCounts mustBe Vector(8, 6, 3)
        result.pairs mustBe Vector.fill(8)((0, 0)) ++ Vector.fill(6)((1, 1)) ++ Vector.fill(3)((2, 2))
        result.pairs.size + 3 mustBe 20
      }
    }

    "average tied ranks and allocate equal remainders in species order" in {
      given Random = Random(42)
      (for
        selection <- SpeciesSelection.make[IO, Int]
        tied      <- selection.select(singletons(5.0, 5.0, -1.0), 20, 0.0)
        zero      <- selection.select(singletons(0.0, 0.0, 0.0), 10, 0.0)
        negative  <- selection.select(singletons(-1.0, -2.0, -3.0), 20, 0.0)
      yield (tied, zero, negative)).asserting { case (tied, zero, negative) =>
        tied.map(_.offspringCounts) mustBe Right(Vector(7, 7, 3))
        zero.map(_.offspringCounts) mustBe Right(Vector(3, 2, 2))
        negative.map(_.offspringCounts) mustBe Right(Vector(8, 6, 3))
      }
    }

    "rank champions independently of the order species are supplied in" in {
      given Random = Random(42)
      SpeciesSelection.make[IO, Int].flatMap(_.select(singletons(1.0, 10.0, 5.0), 20, 0.0)).asserting { result =>
        result.map(_.offspringCounts) mustBe Right(Vector(3, 8, 6))
      }
    }

    "produce one ordered pair per child and restrict ordinary mating to its species" in {
      given Random = Random(42)
      val low      = evaluated(0.0 -> 1.0, 1.0 -> 2.0)
      val high     = evaluated(10.0 -> 1.0, 11.0 -> 2.0)
      val groups   = SpeciesPopulation(Vector(Species(low.last, low), Species(high.last, high)))
      SpeciesSelection.make[IO, Double].flatMap(_.select(groups, 7, 0.0)).asserting { outcome =>
        val result = outcome.value
        result.offspringCounts mustBe Vector(3, 2)
        result.pairs mustBe Vector.fill(3)((1.0, 1.0)) ++ Vector.fill(2)((11.0, 11.0))
      }
    }

    "keep the first parent local and draw the second from another species when interspecies mating is enabled" in {
      given Random = Random(42)
      SpeciesSelection.make[IO, Int].flatMap(_.select(singletons(5.0, 1.0), 8, 1.0)).asserting { outcome =>
        val result     = outcome.value
        val localCount = result.offspringCounts.head
        result.pairs.take(localCount).forall(_ == (0, 1)) mustBe true
        result.pairs.drop(localCount).forall(_ == (1, 0)) mustBe true
        result.pairs.size mustBe 6
      }
    }

    "retain a size-one population without requesting offspring" in {
      given Random = Random(42)
      SpeciesSelection.make[IO, Int].flatMap(_.select(singletons(1.0), 1, 1.0)).asserting { outcome =>
        val result = outcome.value
        result.offspringCounts mustBe Vector(0)
        result.pairs mustBe empty
      }
    }

    "reject quotas that cannot preserve a representative and a child per species" in {
      given Random = Random(42)
      SpeciesSelection
        .make[IO, Int]
        .flatMap { selection =>
          Vector(
            selection.select(singletons(1.0, 0.0), 3, 0.0),
            selection.select(singletons(1.0), 0, 0.0),
            selection.select(SpeciesPopulation(Vector.empty), 10, 0.0),
            selection.select(SpeciesPopulation(Vector(Species(0 -> Fitness(1.0), Vector.empty))), 2, 0.0),
            selection.select(singletons(1.0), 2, Double.NaN),
            selection.select(singletons(1.0), 2, 1.1),
            selection.select(singletons(1.0), 2, -0.1)
          ).sequence
        }
        .asserting { outcomes =>
          outcomes.map(_.left.value.getMessage) mustBe Vector(
            "Population must fit a representative and a child per species",
            "Population size must be positive",
            "Breeding requires nonempty species",
            "Breeding requires nonempty species",
            "Interspecies probability must be between zero and one",
            "Interspecies probability must be between zero and one",
            "Interspecies probability must be between zero and one"
          )
          summon[Random].nextLong() mustBe new Random(42).nextLong()
        }
    }
  }

  "SpeciesConservation" should {
    "reserve weaker representatives and fill globally while preserving species order" in {
      val species = SpeciesPopulation(Vector(group(1.0 -> 1.0), group(0.0 -> 10.0, 0.1 -> 9.0, 0.2 -> 8.0)))
      SpeciesConservation.make[IO, Double].flatMap(_.conserve(species, 3)).asserting { result =>
        result mustBe Right(SpeciesPopulation(Vector(group(1.0 -> 1.0), group(0.0 -> 10.0, 0.1 -> 9.0))))
      }
    }

    "reserve one occurrence and retain other legitimate copies within its species" in {
      val species = SpeciesPopulation(Vector(group(0.0 -> 10.0, 0.0 -> 10.0, 0.1 -> 9.0), group(1.0 -> 1.0)))
      SpeciesConservation.make[IO, Double].flatMap(_.conserve(species, 3)).asserting { result =>
        result mustBe Right(SpeciesPopulation(Vector(group(0.0 -> 10.0, 0.0 -> 10.0), group(1.0 -> 1.0))))
        val conserved = result.value
        conserved.population mustBe Vector(0.0, 0.0, 1.0)
        conserved.distinctCandidates mustBe 2
        conserved.sizes mustBe Vector(2, 1)
        conserved.representatives mustBe Vector(0.0, 1.0)
      }
    }

    "allocate identical occurrences to their original groups without merging them" in {
      val species = SpeciesPopulation(Vector(group(0.0 -> 10.0, 0.0 -> 10.0), group(0.0 -> 10.0)))
      SpeciesConservation
        .make[IO, Double]
        .flatMap { conservation =>
          (conservation.conserve(species, 2), conservation.conserve(species, 3)).tupled
        }
        .asserting { case (two, three) =>
          two mustBe Right(SpeciesPopulation(Vector(group(0.0 -> 10.0), group(0.0 -> 10.0))))
          three mustBe Right(species)
        }
    }

    "keep the best representatives when the limit is smaller than the number of groups and drop empty groups" in {
      val species = SpeciesPopulation(Vector(group(1.0 -> 1.0), group(0.0 -> 10.0), group(2.0 -> 8.0)))
      (for
        conservation <- SpeciesConservation.make[IO, Double]
        small        <- conservation.conserve(species, 2)
        large        <- conservation.conserve(species, 25)
        zero         <- conservation.conserve(species, 0)
      yield (small, large, zero)).asserting { case (small, large, zero) =>
        small mustBe Right(SpeciesPopulation(Vector(group(0.0 -> 10.0), group(2.0 -> 8.0))))
        large mustBe Right(species)
        zero mustBe Right(SpeciesPopulation(Vector.empty))
      }
    }

    "resolve tied fill slots by occurrence order and retain the highest member as representative" in {
      val species = SpeciesPopulation(Vector(group(0.1 -> 4.0, 0.0 -> 5.0), group(1.0 -> 5.0, 1.1 -> 4.0)))
      SpeciesConservation.make[IO, Double].flatMap(_.conserve(species, 3)).asserting { result =>
        result mustBe Right(SpeciesPopulation(Vector(group(0.0 -> 5.0, 0.1 -> 4.0), group(1.0 -> 5.0))))
      }
    }

    "accept an empty collection of species" in
      SpeciesConservation
        .make[IO, Double]
        .flatMap(_.conserve(SpeciesPopulation(Vector.empty), 3))
        .asserting(_ mustBe Right(SpeciesPopulation(Vector.empty)))

    "return a typed error for a negative limit" in
      SpeciesConservation.make[IO, Double].flatMap(_.conserve(SpeciesPopulation(Vector(group(0.0 -> 1.0))), -1)).asserting { result =>
        result.left.value.getMessage mustBe "Conserved population size must be nonnegative"
      }

    "return a typed error when a representative has no member occurrence to reserve" in
      SpeciesConservation
        .make[IO, Double]
        .flatMap { conservation =>
          Vector(
            Species(0.0 -> Fitness(1.0), Vector.empty),
            Species(0.0 -> Fitness(1.0), evaluated(1.0 -> 2.0))
          ).traverse(species => conservation.conserve(SpeciesPopulation(Vector(species)), 1))
        }
        .asserting { results =>
          results.map(_.left.value.getMessage) mustBe Vector.fill(2)("Every species representative must be one of its members")
        }
  }

  "SpeciesOperators.make" should {
    "assemble operators sharing the supplied search distance" in
      SpeciesOperators.make[IO, Double](distance).flatMap(_.speciation.partition(evaluated(0.0 -> 2.0, 1.0 -> 1.0), 0.15, 8)).asserting {
        _.map(_.sizes) mustBe Right(Vector(1, 1))
      }
  }
}
