package currexx.algorithms.operators

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import currexx.algorithms.Fitness

import scala.util.Random

class SelectorSpec extends IOWordSpec {

  "Selector.rouletteWheel" should {

    "distribute evaluated population into pairs" in {
      val population = Vector(
        (7, Fitness(35.0)), // 1
        (6, Fitness(20.0)), // 2
        (4, Fitness(10.0)), // 5
        (8, Fitness(7.0)),  // 3
        (3, Fitness(5.0)),  // 4
        (1, Fitness(4.0)),  // 6
        (2, Fitness(2.0)),
        (5, Fitness(1.0)),
        (10, Fitness(-1.0)),
        (9, Fitness(-7.0))
      )

      given Random = Random(42)
      val result   = Selector.rouletteWheel[IO, Int].flatMap(_.selectPairs(population, 6))

      result.asserting(_ mustBe Vector((4, 6), (7, 7), (6, 8)))
    }
  }

  "Selector.tournament" should {

    val population = Vector(
      (7, Fitness(35.0)),
      (6, Fitness(20.0)),
      (4, Fitness(10.0)),
      (8, Fitness(7.0)),
      (3, Fitness(5.0)),
      (1, Fitness(4.0)),
      (2, Fitness(2.0)),
      (5, Fitness(1.0)),
      (10, Fitness(-1.0)),
      (9, Fitness(-7.0))
    )

    def selected(pairs: Vector[(Int, Int)]): Vector[Int] =
      pairs.flatMap { case (first, second) => Vector(first, second) }

    "distribute evaluated population into pairs" in {
      given Random = Random(42)

      val result = Selector.tournament[IO, Int].flatMap(_.selectPairs(population, 6))

      result.asserting { pairs =>
        pairs must have size 3
        selected(pairs).toSet.subsetOf(population.map(_._1).toSet) mustBe true
      }
    }

    "keep selecting with replacement once every individual has been drawn" in {
      given Random = Random(42)

      // Selection with replacement is the entire source of selection pressure. Drawing without it would cap the
      // result at one appearance per individual, so asking for as many parents as there are individuals — which is
      // exactly what the GA does — would make fitness almost irrelevant to the next generation.
      val result = Selector.tournament[IO, Int].flatMap(_.selectPairs(population, population.size * 2))

      result.asserting(selected(_) must have size (population.size * 2))
    }

    "select fitter individuals more often than weaker ones" in {
      given Random = Random(42)

      val result = Selector.tournament[IO, Int].flatMap(_.selectPairs(population, 2000))

      result.asserting { pairs =>
        val fitnessOf      = population.toMap
        val chosen         = selected(pairs)
        val meanChosen     = chosen.map(fitnessOf(_).value).sum / chosen.size
        val meanPopulation = population.map(_._2.value).sum / population.size

        meanChosen must be > meanPopulation
      }
    }
  }
}
